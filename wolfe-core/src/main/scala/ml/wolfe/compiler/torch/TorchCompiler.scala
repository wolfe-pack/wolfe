package ml.wolfe.compiler.torch

import java.io.{File, PrintWriter}
import java.util.UUID

import breeze.linalg.DenseMatrix
import ml.wolfe.Language._
import ml.wolfe._
import ml.wolfe.compiler.{DelayedCompiler, Module}
import ml.wolfe.term.{Var, _}
import org.scalautils.{Accumulation, Every, Good, Or}
import Accumulation._

import scala.collection.mutable


/**
 * @author riedel
 */
object TorchCompiler extends DelayedCompiler {

  sealed trait TorchTerm[+T] extends Term[T]

  object nn {

    case class Linear(weight: ParamSelector[Tensor], bias: ParamSelector[Tensor],
                      arg: Term[Tensor], in: Int, out: Int) extends TorchTerm[Tensor]

  }

  case class ParamSelector[+T](param: Var[Any], term: Term[T], path: List[Term[Any]]) extends TorchTerm[T] {
    override def toString = pathString(path)
  }

  def pathString(path: List[Term[Any]]) =
    path.map {
      case v: Var[_] => v.name
      case GetElement(_, element) => "[" + element + "]"
    }.mkString(".")


  case class CompilationContext(paramBindings: Bindings,
                                inputBindings: Bindings,
                                domains: Domains = Domains()) {

    object SelectorPattern {
      def unapply[T](term: Term[T]): Option[ParamSelector[T]] = term match {
        case v: Var[_] if paramBindings.contains(v) => Some(ParamSelector(v, v, v :: Nil))
        case GetElement(SelectorPattern(ParamSelector(v, _, path)), _) => Some(ParamSelector(v, term, term :: path))
        case _ => None
      }
    }

  }


  def preCompile[T](term: Term[T], context: CompilationContext): Term[T] Or Every[CompilationError] = {

    def c[A](term: Term[A]) = preCompile(term, context)

    import context._

    term match {
      case ComponentPlus(TensorMul(SelectorPattern(weight), arg), SelectorPattern(bias)) =>
        context.domains(weight.term) match {
          case TensorDom(List(d1, d2)) => for (t <- c(arg)) yield nn.Linear(weight, bias, t, d1, d2)
          case TensorDom(List(d1)) => for (t <- c(arg)) yield nn.Linear(weight, bias, t, d1, 1)
          case _ => Good(term)
        }

      case cp: ComposedProduct =>
        for (args <- (cp.parts map c).combined) yield cp.clone(args)

      case _ => Good(term)
    }
  }

  case class LuaVariableAndDef(variable: String, definition: String)

  class NameGenerator {
    val counts = new mutable.HashMap[String, Int] withDefaultValue -1

    def newName(prefix: String) = {
      counts(prefix) += 1
      prefix + counts(prefix)
    }
  }

  def stackNodes(arg: Term[Any], namePrefix: String, luaExpr: String => String)
                (implicit context: CompilationContext, generator: NameGenerator) = {
    val result = compileToLua(arg)
    val variable = generator.newName(namePrefix)
    val inputNodes = arg match {
      case v: Var[_] => result.inputNodes + (v -> variable)
      case _ => result.inputNodes
    }
    val definition =
      s"${if (result.definition != "") result.definition + "\n" else ""}$variable = ${luaExpr(result.varName)}"
    result.copy(variable, definition, inputNodes)
  }

  case class LuaCompilationResult(varName: String, definition: String,
                                  inputNodes: Map[Var[Any], String],
                                  paramExpressions: Map[Var[Any], String] = Map.empty,
                                  linearUnits: List[(nn.Linear, String)] = Nil)


  def compileToLua(term: Term[Any])(implicit context: CompilationContext,
                                    generator: NameGenerator): LuaCompilationResult = {
    term match {
      case lin@nn.Linear(_, _, arg, in, out) =>
        val result = stackNodes(arg, "linear", a => s"nn.Linear($in,$out)($a)")
        result.copy(linearUnits = (lin -> result.varName) :: result.linearUnits)

      case Sigmoid(arg) =>
        stackNodes(arg, "sigm", a => s"nn.Sigmoid()($a)")

      case v: Var[_] => LuaCompilationResult("", "", Map.empty)
    }
  }

  val preamble =
    """
      |require 'nn'
      |require 'nngraph'
    """.stripMargin

  case class ParameterMapping(param: Var[Any],
                              initFunName: String, initFunDef: String,
                              gradFunName: String, gradFunDef: String)

  def compile[T](term: Term[T], paramBindings: Bindings, inputBindings: Bindings) = {
    val variableDomainBindings = (paramBindings ++ inputBindings) map { b => b.variable in Typer.deriveDomainFromValue(b.value) }
    val variableDomains = Domains(variableDomainBindings.toSeq: _*)

    for (domains <- Typer.domains(variableDomains)(term);
         context = CompilationContext(paramBindings, inputBindings, domains);
         precompiled <- preCompile(term, context)) yield {

      println(precompiled)
      val nameGenerator = new NameGenerator
      val compilationResult = compileToLua(precompiled)(context, nameGenerator)

      val parameterMapping = (for (param <- paramBindings) yield {
        val weightModules = compilationResult.linearUnits.filter(_._1.weight.param == param.variable)
        val biasModules = compilationResult.linearUnits.filter(_._1.bias.param == param.variable)
        val weightUpdates = for ((lin, name) <- weightModules) yield {
          s"$name.data.module.weight = ${lin.weight}"
        }
        val biasUpdates = for ((lin, name) <- biasModules) yield {
          s"$name.data.module.bias = ${param.variable.name}"
        }

        val initName = "init" + param.variable.name
        val initDef =
          s"""
             |function $initName(${param.variable.name})
             |  ${(weightUpdates ++ biasUpdates).mkString("\n  ")}
             |end
        """.stripMargin

        val gradName = "grad" + param.variable.name
        val gradResult = if (weightModules.nonEmpty) weightModules.head._2 + ".data.module.gradWeight"
        else
          biasModules.head._2 + ".data.module.gradBias"
        val gradDef =
          s"""
             |function $gradName()
             |  return $gradResult
             |end
          """.stripMargin

        param.variable -> ParameterMapping(param.variable, initName, initDef, gradName, gradDef)
      }).toMap


      val gmod = "gmod"
      val initFunctionsDef = parameterMapping.values.map(_.initFunDef).mkString("\n")
      val gradFunctionsDef = parameterMapping.values.map(_.gradFunDef).mkString("\n")

      val orderedInputNodes = compilationResult.inputNodes.toSeq
      val forwardDef = {
        val forwardArgs = if (orderedInputNodes.size == 1) orderedInputNodes.head._1.name
        else
          orderedInputNodes.map(_._1.name).mkString("{", ", ", "}")
        s"""
           |function forward(${orderedInputNodes.map(_._1.name).mkString(",")})
           |  lastForwardArguments = $forwardArgs
           |  $gmod:forward(lastForwardArguments)
           |end
        """.stripMargin
      }
      val outputDef =
        s"""
           |function output()
           |  return $gmod.output
           |end
        """.stripMargin

      val backwardDef =
        s"""
           |function backward(gradOutput)
           |  $gmod:backward(lastForwardArguments,gradOutput)
           |end
        """.stripMargin


      val script =
        s"""
           |$preamble
           |${compilationResult.definition}
           |$gmod = nn.gModule({${orderedInputNodes.map(_._2).mkString(",")}},{${compilationResult.varName}})
           |$initFunctionsDef
           |$gradFunctionsDef
           |$forwardDef
           |$backwardDef
           |$outputDef
        """.stripMargin

      val client = new TorchZeroMQClient()
      val uuid = UUID.randomUUID().toString
      val scriptFile = File.createTempFile(uuid, ".lua")
      val scriptOut = new PrintWriter(scriptFile)
      logger.info(s"Creating lua file: $scriptFile")
      scriptOut.println(script)
      scriptOut.close()
      client.call("dofile")(scriptFile.getAbsolutePath)
      new Module[T] {


        def gradient[G](param: Var[G]) = {
          //for the given parameter, find the linear unit that owns it
          val gradName = parameterMapping(param).gradFunName
          client.call(gradName)().asInstanceOf[G]
        }

        def init(bindings: Binding[Any]*) = {
          for (binding <- bindings) {
            val funName = parameterMapping(binding.variable).initFunName
            client.call(funName)(binding.value)
          }
        }

        def forward(bindings: Binding[Any]*) = {
          val lastForwardBinding = Bindings(bindings: _*)
          val args = orderedInputNodes.map(p => lastForwardBinding(p._1))
          client.call("forward")(args: _*)
        }

        def output() = {
          client.call("output")().asInstanceOf[T]
        }

        def backward(output: T) = {
          client.call("backward")(output)
        }
      }
    }


  }

  def main(args: Array[String]) {
    val W = Var[Tensor]("W")
    val x = Var[Tensor]("x")
    val b = Var[Tensor]("b")
    val term = sigmoid(W * x + b)

    val module = TorchCompiler.compile(term)
    module.init(W := DenseMatrix.ones(2, 2), b := DenseMatrix(1.0, 2.0))
    module.forward(x := DenseMatrix(1.0, 1.0))

    println(module.output())
    println(module.output().isInstanceOf[DenseMatrix[_]])

    module.backward(DenseMatrix(1.0, 1.0))

    println(module.gradient(W))

  }
}
