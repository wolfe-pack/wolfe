package scalapplcodefest.compiler

import scala.tools.nsc.Global
import java.io.FileWriter
import scalapplcodefest.Wolfe.{Objective, Output}
import scala.annotation.StaticAnnotation
import scalapplcodefest.Wolfe.Objective.Differentiable

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

  private def checkAnnotation[T <: Global#Tree](fun: Global#DefDef, annotation: StaticAnnotation): Boolean =
    fun.symbol.annotations.map(_.toString).exists(s => s.startsWith(annotation.getClass.getName.replace('$','.')))

  def transformTree[T <: Global#Tree](global: Global, env: WolfeCompilerPlugin2#Environment, tree: T) = {
    import global._

    tree match {
      //we are interested in function definitions with annotations
      case fun @ DefDef(_, _, _, _, _, rhs) if checkAnnotation(fun, new Differentiable) =>
        val python = pythify(rhs.toString())
        println("Found differentiable objective!")
        println(s"scala objective:      $rhs")
        println(s"python objective:     $python")
        println(s"python differential:  ${python.differentiate()}")
        //TODO: replace rhs with scala function that calculates the differential
      case _ => //
    }

    tree
  }

  def pythify(code: String): String = code
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
  println(s"f dx: ${f.diff('x)}")
  println(s"f dy: ${f.diff('y)}")
  println()

  val g = "1 / (1 + exp(z))"
  println(s"g:    ${g.function}")
  println(s"g dz: ${g.differentiate()}")
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

  def diff(symbol: String): String = {
    generateScript(symbol)
    run
  }

  def diff(symbol: Symbol): String = diff(symbol.name)

  def differentiate(): String = {
    require(symbols.size == 1)
    diff(symbols.head)
  }
}

object MathASTSandbox extends App {
  //vectors: http://docs.sympy.org/dev/modules/physics/mechanics/vectors.html
  //tensors: http://docs.sympy.org/0.7.0/modules/tensor.html

  //BEGIN SCALA CODE
  import math._
  import scalapplcodefest.Wolfe.Objective

  @Objective.Differentiable
  @Output.LaTeX
  def sigmoidScala(z: Double) = 1 / (1 + exp(-z))
  //END

  val sigmoidScalaCode =
    """
      |import math._
      |import scalapplcodefest.Wolfe.Objective
      |import scalapplcodefest.Wolfe.Output
      |
      |@Objective.Differentiable
      |@Output.LaTeX
      |def sigmoidScala(z: Double) = 1 / (1 + exp(-z))
    """.stripMargin

  private def dirPathOfClass(className: String) = try {
    val resource = className.split('.').mkString("/", "/", ".class")
    val path = getClass.getResource(resource).getPath
    val root = path.dropRight(resource.length)
    root
  }

  val additionalClassPath = List(dirPathOfClass(getClass.getName))

  ASTExplorer.exploreAST(sigmoidScalaCode, List("parser"), showBrowser = true,
    additionalClassPath = additionalClassPath, transformers = List(new DerivativeTransformer))
}