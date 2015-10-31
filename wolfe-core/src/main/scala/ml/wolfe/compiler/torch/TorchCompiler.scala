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

    case class Linear(weight: Var[Tensor], bias: Var[Tensor], arg: Term[Tensor], in: Int, out: Int) extends TorchTerm[Tensor]

  }

  def preCompile[T](term: Term[T], domains: Domains): Term[T] Or Every[CompilationError] = {

    def c[A](term: Term[A]) = preCompile(term, domains)

    term match {
      case ComponentPlus(TensorMul(v: Var[_], arg), bias: Var[_]) =>
        domains(v) match {
          case TensorDom(List(d1, d2)) => for (t <- c(arg)) yield nn.Linear(v, bias, t, d1, d2)
          case TensorDom(List(d1)) => for (t <- c(arg)) yield nn.Linear(v, bias, t, d1, 1)
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
                (implicit context: LuaCompilationContext, generator: NameGenerator) = {
    val result = compileToLua(arg)
    val variable = generator.newName(namePrefix)
    val inputNodes = arg match {
      case v: Var[_] => result.inputNodes + (v -> variable)
      case _ => result.inputNodes
    }
    val definition =
      s"${if (result.definition != "") result.definition + "\n" else ""}$variable = ${luaExpr(result.varName)}"
    result.copy(variable,definition,inputNodes)
  }

  case class LuaCompilationResult(varName: String, definition: String,
                                  inputNodes: Map[Var[Any], String],
                                  paramExpressions: Map[Var[Any], String] = Map.empty,
                                  linearUnits: List[(nn.Linear, String)] = Nil)

  case class LuaCompilationContext(paramBindings: Bindings,
                                   inputBindings: Bindings)

  def compileToLua(term: Term[Any])(implicit context: LuaCompilationContext,
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


  def compile[T](term: Term[T], paramBindings: Bindings, inputBindings: Bindings) = {
    val variableDomainBindings = (paramBindings ++ inputBindings) map { b => b.variable in Typer.deriveDomainFromValue(b.value) }
    val variableDomains = Domains(variableDomainBindings.toSeq: _*)

    for (domains <- Typer.domains(variableDomains)(term);
         precompiled <- preCompile(term, domains)) yield {

      println(precompiled)
      val nameGenerator = new NameGenerator
      val compilationResult = compileToLua(precompiled)(LuaCompilationContext(paramBindings, inputBindings), nameGenerator)

      val initFunctions = (for (param <- paramBindings) yield {
        val funName = "init" + param.variable.name
        val weightUpdates = for ((lin, name) <- compilationResult.linearUnits.filter(_._1.weight == param.variable)) yield {
          s"$name.weight = ${param.variable.name}"
        }
        val biasUpdates = for ((lin, name) <- compilationResult.linearUnits.filter(_._1.bias == param.variable)) yield {
          s"$name.bias = ${param.variable.name}"
        }

        val fun =
          s"""
             |function $funName(${param.variable.name})
             |  ${(weightUpdates ++ biasUpdates).mkString("\n  ")}
             |end
        """.stripMargin
        param.variable -> (funName, fun)
      }).toMap
      val gmod = "gmod"
      val initFunctionsDef = initFunctions.values.map(_._2).mkString("\n")
      val orderedInputNodes = compilationResult.inputNodes.toSeq
      val forwardFunctionDef = {
        val forwardArgs = if (orderedInputNodes.size == 1) orderedInputNodes.head._1.name else
          orderedInputNodes.map(_._1.name).mkString("{",", ", "}")
        s"""
          |function forward(${orderedInputNodes.map(_._1.name).mkString(",")})
          |  $gmod:forward($forwardArgs)
          |end
        """.stripMargin
      }
      val outputDef =
        s"""
          |function output()
          |  return $gmod.output
          |end
        """.stripMargin

      val script =
        s"""
           |$preamble
           |${compilationResult.definition}
           |$gmod = nn.gModule({${orderedInputNodes.map(_._2).mkString(",")}},{${compilationResult.varName}})
           |$initFunctionsDef
           |$forwardFunctionDef
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
        def gradient[G](param: Var[G]) = ???

        def init(bindings: Binding[Any]*) = {
          for (binding <- bindings) {
            val funName = initFunctions(binding.variable)._1
            client.call(funName)(binding.value)
          }
        }

        def forward(bindings: Binding[Any]*) = {
          val bindingMap = Bindings(bindings:_*)
          val args = orderedInputNodes.map(p => bindingMap(p._1))
          client.call("forward")(args:_*)
        }

        def output() = {
          client.call("output")().asInstanceOf[T]
        }

        def backward(output: T) = ???
      }
    }


  }

  def main(args: Array[String]) {
    val W = Var[Tensor]("W")
    val x = Var[Tensor]("x")
    val b = Var[Tensor]("b")
    val term = sigmoid(W * x + b)

    val module = TorchCompiler.compile(term)
    module.init(W := DenseMatrix.ones(2, 2), b := DenseMatrix(0.0, 0.0))
    module.forward(x := DenseMatrix(1.0, 2.0))

    println(module.output())
    println(module.output().isInstanceOf[DenseMatrix[_]])

  }
}
