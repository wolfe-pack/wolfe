package scalapplcodefest.compiler

import scala.tools.nsc.Global
import java.io.FileWriter

/**
 * User: rockt
 * Date: 1/4/14
 * Time: 10:27 AM
 */

/**
 * Searches for @Differentiable annotation and generates an AST for gradient calculation
 */
class DerivativeTransformer extends WolfeTransformer {
  def transformTree[T <: Global#Tree](global: Global, env: WolfeCompilerPlugin2#Environment, tree: T) = tree match {
    case _ => tree //TODO
  }
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
  println(s"g dz: ${g.diff('z)}")
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
  private val symbolsDef = symbols.map((s: String) => s"""$s = Symbol("$s")""").mkString("\n")


  private val pathToScript = "/tmp/test.py" //TODO: can we keep that in virtual memory?

  private val stub =
  s"""
    |from sympy import *
    |import numpy as np
    |
    |$symbolsDef
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
}

object MathASTSandbox extends App {
  import SymPyDSL._

  import math._
  val sigmoid = "1 / (1 + exp(-z))"

  println(sigmoid.diff('z))
}