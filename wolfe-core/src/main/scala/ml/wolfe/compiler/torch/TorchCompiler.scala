package ml.wolfe.compiler.torch

import java.io.{File, PrintWriter}
import java.util.UUID

import breeze.linalg.DenseMatrix
import ml.wolfe.Language._
import ml.wolfe._
import ml.wolfe.compiler.{DelayedCompiler, Module}
import ml.wolfe.term.{Var, _}
import org.scalactic.{Accumulation, Every, Good, Or}
import Accumulation._

import scala.collection.mutable


/**
  * Make sure to run 'th src/lua/torch_server.lua' while running the compiled module.
  * @author riedel
  */
object TorchCompiler extends DelayedCompiler {

  sealed trait TorchTerm[+T] extends Term[T]

  object nn {

    case class Linear(weight: ParamSelector[Tensor], bias: ParamSelector[Tensor],
                      arg: Term[Tensor], in: Int, out: Int) extends TorchTerm[Tensor]

  }

  case class ParamSelector[+T](param: Var[Any], term: Term[T], path: List[Term[Any]]) extends TorchTerm[T] {
    override def toString = pathString(path.reverse)
  }

  def pathString(path: List[Term[Any]]) =
    path.map {
      case v: Var[_] => v.name
      case GetElement(_, element) => "[" + (element + 1) + "]"
    }.mkString("")


  case class CompilationContext(paramBindings: Bindings,
                                inputBindings: Bindings,
                                domains: Domains = Domains(),
                                previous: LuaCompilationResult = LuaCompilationResult("", "")) {

    object SelectorPattern {
      def unapply[T](term: Term[T]): Option[ParamSelector[T]] = term match {
        case v: Var[_] if paramBindings.contains(v) => Some(ParamSelector(v, v, v :: Nil))
        case GetElement(SelectorPattern(ParamSelector(v, _, path)), _) => Some(ParamSelector(v, term, term :: path))
        case _ => None
      }
    }

  }


  def preCompile[T](term: Term[T], context: CompilationContext): Term[T] = {

    Transformation.induceAccessors(term, context.paramBindings.contains)
  }

  case class LuaVariableAndDef(variable: String, definition: String)

  def toWolfeObject(value: Any, dom: Dom[Any]): Any = {
    (value, dom) match {
      case (l: List[_], ProductDom(doms, constructor)) =>
        val args = for ((a, d) <- l zip doms) yield toWolfeObject(a, d)
        constructor(args)
      case (l: List[_], SeqDom(elemDom, minLength, _)) =>
        val seq = l.toIndexedSeq.lift
        Range(0, math.max(l.length,minLength)) map (i => seq(i).map(toWolfeObject(_, elemDom)).orNull)
      case _ => value
    }
  }

  class NameGenerator {
    val counts = new mutable.HashMap[String, Int] withDefaultValue -1

    def newName(prefix: String) = {
      counts(prefix) += 1
      prefix + counts(prefix)
    }
  }

  def stackOneNode(arg: Term[Any], namePrefix: String, luaExpr: String => String)
                  (implicit context: CompilationContext, generator: NameGenerator) = {
    stackNodes(Seq(arg), namePrefix, args => luaExpr(args(0)))
  }

  def stackTwoNodes(arg1: Term[Any], arg2: Term[Any], namePrefix: String, luaExpr: (String, String) => String)
                   (implicit context: CompilationContext, generator: NameGenerator) = {
    stackNodes(Seq(arg1, arg2), namePrefix, args => luaExpr(args(0), args(1)))
  }

  def stackNodes(args: Seq[Term[Any]], namePrefix: String, luaExpr: Seq[String] => String)
                (implicit context: CompilationContext, generator: NameGenerator) = {
    val contexts = args.scanLeft(context) {
      case (lastContext, arg) => lastContext.copy(previous = compileToLua(arg)(lastContext, generator))
    }
    val results = contexts.tail.map(_.previous)
    val variable = generator.newName(namePrefix)
    val definition =
      if (results.nonEmpty)
        s"""|${results.map(_.definition).mkString("\n")}
            |$variable = ${luaExpr(results.map(_.varName))}""".stripMargin
      else s"$variable = ${luaExpr(results.map(_.varName))}"

    LuaCompilationResult(variable, definition,
      results.map(_.inputNodes).foldLeft(Map.empty[Var[Any], String])(_ ++ _),
      results.map(_.paramNodes).foldLeft(Map.empty[Var[Any], String])(_ ++ _),
      results.map(_.paramAccessors).foldLeft(Map.empty[VarAccess[Any], String])(_ ++ _),
      results.map(_.linearUnits).foldLeft(List.empty[(nn.Linear, String)])(_ ++ _))
  }


  def tableSignature(dom: Dom[Any]): String = dom match {
    case SeqDom(elemDom, _, maxLength) => Range(0, maxLength).map(_ => tableSignature(elemDom)).mkString("{", ", ", "}")
    case TensorDom(dims) => s"torch.LongStorage({${dims.mkString(", ")}})"
    case ProductDom(doms, _) => doms.map(tableSignature).mkString("{", ", ", "}")
  }

  case class LuaCompilationResult(varName: String, definition: String,
                                  inputNodes: Map[Var[Any], String] = Map.empty,
                                  paramNodes: Map[Var[Any], String] = Map.empty,
                                  paramAccessors: Map[VarAccess[Any], String] = Map.empty,
                                  linearUnits: List[(nn.Linear, String)] = Nil)


  def compileToLua(term: Term[Any])(implicit context: CompilationContext,
                                    generator: NameGenerator): LuaCompilationResult = {
    term match {
      //      case lin@nn.Linear(_, _, arg, in, out) =>
      //        val result = stackNodes(arg, "linear", a => s"nn.Linear($in,$out)($a)")
      //        result.copy(linearUnits = (lin -> result.varName) :: result.linearUnits)

      case Sigmoid(arg) =>
        stackOneNode(arg, "sigm", a => s"nn.Sigmoid()($a)")

      case Log(arg) =>
        stackOneNode(arg, "log", a => s"nn.Log()($a)")

      case GetElement(arg, element) =>
        stackOneNode(arg, "select", a => s"nn.SelectTable(${element + 1})($a)")

      case TensorMul(arg1, arg2) =>
        stackTwoNodes(arg1, arg2, "mm", { case (a1, a2) => s"nn.MM()({$a1, $a2})" })

      case ComponentPlus(arg1, arg2) =>
        stackTwoNodes(arg1, arg2, "plus", { case (a1, a2) => s"nn.CAddTable()({$a1, $a2})" })

      case va@VarAccess(v, path) =>
        def tableString(args: Seq[String]) = if (args.isEmpty) "" else args.mkString("{", ",", "}")
        val pathSpec = path.reverse.drop(1).map {
          case ge@GetElement(_, e) => e.toString
          case _ => "\"?\""
        }.mkString("{", ", ", "}")
        //if there are dynamic elements in the path they should become inputs
        val inputs = path.reverse.drop(1).collect {
          case SeqApply(_, index) => index
        }
        val result = stackNodes(inputs, "paramAccess", args => s"wolfe.ParamAccess($pathSpec)(${tableString(args)})")
        result.copy(paramAccessors = Map(va -> result.varName))

      case v: Var[_] if context.inputBindings.contains(v) =>
        context.previous.inputNodes.get(v) match {
          case Some(varName) => context.previous.copy(varName = varName, definition = "")
          case None =>
            val name = generator.newName("input")
            LuaCompilationResult(name, s"$name = nn.Identity()()", inputNodes = Map(v -> name))
        }
    }
  }

  val preamble =
    """
      |require 'nn'
      |require 'nngraph'
    """.stripMargin

  case class ParameterMapping(param: Var[Any],
                              initFunName: String, initFunDef: String,
                              gradFunName: String, gradFunDef: String,
                              paramFunName: String, paramFunDef: String)

  def compile[T](term: Term[T], paramBindings: Bindings, inputBindings: Bindings) = {
    val variableDomainBindings = (paramBindings ++ inputBindings) map { b => b.variable in Typer.deriveDomainFromValue(b.value) }
    val variableDomains = Domains(variableDomainBindings.toSeq: _*)

    for (domains <- Typer.domains(variableDomains)(term);
         context = CompilationContext(paramBindings, inputBindings, domains);
         precompiled = preCompile(term, context)) yield {

      println(precompiled)
      val nameGenerator = new NameGenerator
      val compilationResult = compileToLua(precompiled)(context, nameGenerator)

      val parameterMapping = (for (param <- paramBindings) yield {
        //val nodeName = compilationResult.paramNodes(param.variable)
        val accessors = compilationResult.paramAccessors.filterKeys(_.variable == param.variable).values.toSeq
        val initName = "init_" + param.variable.name
        val headNodeName = accessors.head + ".data.module"
        val sharing = for (other <- accessors.tail) yield s"$headNodeName:shareWeight($other.data.module)"
        val setupParams =
          s"""
             |local dims = ${tableSignature(domains(param.variable))}
             |$headNodeName:initWeight(dims)
             |${sharing.mkString("\n")}
           """.stripMargin
        val setAll = for (mod <- accessors) yield s"$mod.data.module.weight = ${param.variable.name}"
        val initDef =
          s"""
             |$setupParams
             |function $initName(${param.variable.name})
             |  print("Init:")
             |  print(${param.variable.name})
             |  ${setAll.mkString("\n  ")}
             |end
        """.stripMargin

        val gradName = "grad_" + param.variable.name
        val modules = accessors.map(_ + ".data.module").mkString("{", ",", "}")
        val gradDef =
          s"""
             |function $gradName()
             |  return wolfe.aggregateGradients($modules)
             |end
          """.stripMargin

        val paramName = "param_" + param.variable.name
        val paramDef =
          s"""
             |function $paramName()
             |  return $headNodeName.weight
             |end
          """.stripMargin

        param.variable -> ParameterMapping(param.variable, initName, initDef, gradName, gradDef, paramName, paramDef)
      }).toMap


      val gmod = "gmod"
      val initFunctionsDef = parameterMapping.values.map(_.initFunDef).mkString("\n")
      val gradFunctionsDef = parameterMapping.values.map(_.gradFunDef).mkString("\n")
      val paramFunctionsDef = parameterMapping.values.map(_.paramFunDef).mkString("\n")

      val inputNodes = compilationResult.inputNodes.toSeq
      val inputNodeNames = inputNodes.map(_._2)
      val paramNodeNames = compilationResult.paramAccessors.filterNot(_._1.isDynamic).values.toSeq
      val mixedInputNodes = paramNodeNames ++ inputNodeNames
      val numParams = paramNodeNames.length
      val numInputs = inputNodeNames.length
      //val inputNames = orderedInputNodes.map(_._1.name).mkString(",")

      val forwardDef =
        s"""
           |local lastForwardArguments = {}
           |-- initializing the empty dummy inputs to parameters
           |for i=1, $numParams do
           |  lastForwardArguments[i] = {}
           |end
           |function forward(input)
           |  for i=${numParams + 1}, ${numParams + numInputs} do
           |    lastForwardArguments[i] = input[i - $numParams]
           |  end
           |  print("forward")
           |  print(lastForwardArguments)
           |  $gmod:forward(lastForwardArguments)
           |end
        """.stripMargin

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

      val updateCalls = compilationResult.paramAccessors.values.toSeq.sorted.map(
        n => s"$n.data.module:updateParameters(-learningRate)")
      val updateDef =
        s"""
           |function updateParameters(learningRate)
           |  ${updateCalls.mkString("\n  ")}
           |end
         """.stripMargin

      val script =
        s"""
           |$preamble
           |${compilationResult.definition}
           |$gmod = nn.gModule({${mixedInputNodes.mkString(",")}},{${compilationResult.varName}})
           |$initFunctionsDef
           |$gradFunctionsDef
           |$paramFunctionsDef
           |$forwardDef
           |$backwardDef
           |$outputDef
           |$updateDef
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
          val ret = client.call(gradName)()
          val dom = domains(param)
          toWolfeObject(ret, dom).asInstanceOf[G]
        }

        def param[P](param: Var[P]) = {
          val paramName = parameterMapping(param).paramFunName
          val ret = client.call(paramName)()
          val dom = domains(param)
          toWolfeObject(ret, dom).asInstanceOf[P]
        }

        def init(bindings: Binding[Any]*) = {
          for (binding <- bindings) {
            val funName = parameterMapping(binding.variable).initFunName
            client.call(funName)(binding.value)
          }
        }

        def forward(bindings: Binding[Any]*) = {
          val lastForwardBinding = Bindings(bindings: _*)
          val args = inputNodes.map(p => lastForwardBinding(p._1))
          client.call("forward")(args)
        }

        def output() = {
          client.call("output")().asInstanceOf[T]
        }

        def backward(output: T) = {
          client.call("backward")(output)
        }

        def updateParameters(learningRate: Double) = {
          client.call("updateParameters")(learningRate)
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
