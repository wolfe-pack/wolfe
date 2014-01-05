package scalapplcodefest.compiler

import scala.tools.nsc.Global
import java.io.FileWriter
import scalapplcodefest.Wolfe.Output
import scala.annotation.StaticAnnotation
import scalapplcodefest.Wolfe.Objective.{Wolferine, Differentiable}

/**
 * User: rockt
 * Date: 1/4/14
 * Time: 10:27 AM
 */

/**
 * Searches for @Objective.Differentiable annotation and generates an AST for gradient calculation
 */
class DerivativeTransformer extends WolfeTransformer {
  import SymPyDSL._

  private def isDifferentiable[T <: Global#Tree](fun: Global#DefDef, annotation: StaticAnnotation): Boolean =
    fun.symbol.annotations.map(_.toString).exists(s => s.startsWith(annotation.getClass.getName.replace('$','.')))

  def transformTree[T <: Global#Tree](global: Global, env: WolfeCompilerPlugin2#Environment, tree: T) = {
    import global._

    tree match {
      //we are interested in function definitions with annotations
      case fun @ DefDef(_, _, _, _, _, rhs) if isDifferentiable(fun, new Differentiable) =>
        val python = pythify(rhs.toString())
        println(
          s"""
            |===== Found differentiable function: ${fun.name} =====
            | scala objective:     $rhs
            | python objective:    $python
            | python differential: ${python.differential()}
          """.stripMargin)
        //TODO: replace rhs with scala function that represents the differential
      case _ => //
    }

    tree
  }

  //rockt: this could be generalized (e.g. for toLaTeX, toPython etc.)
  private def scalaToPython[T <: Global#Tree](global: Global, env: WolfeCompilerPlugin2#Environment, tree: T): String = {
    import global._
    def toPython(tree: T, python: String): String = tree match {
      case Apply(fun, args) => "" //TODO
      case _ => ""
    }
    toPython(tree, "")
  }

  /**
   * Shallow conversion from Scala to Python
   */
  @deprecated
  private def pythify(code: String): String = code
    .replace("scala.math.`package`.", "")
    .replace(".unary_-", "*(-1)")
    .replace(".", "")
}

object DerivativeTransformerPlayground extends App {
  import SymPyDSL._
  //TODO

  //from AST
  val f = "x**3 + 4 * x**2 + 1 + 3 * y**4 * x"

  println(s"f:    ${f.function}")
  println(s"f dx: ${f.differential('x)}")
  println(s"f dy: ${f.differential('y)}")
  println()

  val g = "1 / (1 + exp(z))"
  println(s"g:    ${g.function}")
  println(s"g dz: ${g.differential()}")
  //build AST

  //TODO
}

object SymPyDSL {
  implicit def stringToSymPyDerivator(function: String): SymPyDerivator = SymPyDerivator(function)
}

case class SymPyDerivator(function: String) {
  import scala.sys.process.Process

  val functionNames = Set(
    "exp", "log", "min", "max"
  )

  private val symbols = function.split("[^a-zA-Z]").toSet.filterNot(_.isEmpty).filterNot(functionNames.contains)
  private val symbolsPythonDefString = symbols.map((s: String) => s"""$s = Symbol("$s")""").mkString("\n")


  private val pathToScript = "/tmp/test.py" //TODO: can we keep that in virtual memory?

  private val stub =
  s"""
    |from sympy import *
    |import numpy as np
    |
    |$symbolsPythonDefString
    |f = $function
  """.stripMargin


  private def generateScript(symbol: String): Unit = {
    val code = stub +
    s"""
      |fprime = f.diff($symbol)
      |print fprime
    """.stripMargin

    val writer = new FileWriter(pathToScript)
    code foreach (writer.write(_))
    writer.close()
  }

  private def run = {
    //last character is linebreak
    Process("python", Seq(pathToScript)).!!.init
  }

  def differential(symbol: String): String = {
    generateScript(symbol)
    run
  }

  def differential(symbol: Symbol): String = differential(symbol.name)

  def differential(): String = {
    require(symbols.size == 1)
    differential(symbols.head)
  }
}

object MathASTSandbox extends App {
  //vectors: http://docs.sympy.org/dev/modules/physics/mechanics/vectors.html
  //tensors: http://docs.sympy.org/0.7.0/modules/tensor.html

  //BEGIN SCALA CODE
  import math._
  import scalapplcodefest.Wolfe.Output
  import scalapplcodefest.Wolfe.Objective
  import scalapplcodefest.Wolfe.Objective.Wolferine

  @Objective.Differentiable(differentiator = Some(Wolferine))
  @Output.LaTeX
  def sigmoid(z: Double) = 1 / (1 + exp(-z))
  //END

  val sigmoidCode =
    """
      |  import math._
      |  import scalapplcodefest.Wolfe.Output
      |  import scalapplcodefest.Wolfe.Objective
      |  import scalapplcodefest.Wolfe.Objective.Wolferine
      |
      |  @Objective.Differentiable(differentiator = Some(Wolferine))
      |  @Output.LaTeX
      |  def sigmoid(z: Double) = 1 / (1 + exp(-z))
    """.stripMargin

  private def dirPathOfClass(className: String) = try {
    val resource = className.split('.').mkString("/", "/", ".class")
    val path = getClass.getResource(resource).getPath
    val root = path.dropRight(resource.length)
    root
  }

  val additionalClassPath = List(dirPathOfClass(getClass.getName))

  ASTExplorer.exploreAST(sigmoidCode, List("parser"), showBrowser = true,
    additionalClassPath = additionalClassPath, transformers = List(new DerivativeTransformer))
}