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
  //TODO

  //from AST
  val symbol = "x"
  val function = "x**3 + 4 * x**2 + 1 + 3 * y**4 * x"

  println(s"f: $function")

  val sympy = SymPyDerivator(symbol, function)

  val derivative = sympy.execute()

  println(s"f d$symbol: $derivative")
  //build AST

  //TODO
}

case class SymPyDerivator(symbol: String, function: String) {
  import scala.sys.process.Process

  val symbols = function.split("[^a-zA-Z]").toSet.filterNot(_.isEmpty)
  val symbolsDef = symbols.map((s: String) => s"""$s = Symbol("$s")""").mkString("\n")


  val pathToScript = "/tmp/test.py"

  val code =
  s"""
    |from sympy import *
    |import numpy as np
    |
    |$symbolsDef
    |f = $function
    |
    |fprime = f.diff($symbol)
    |print fprime
  """.stripMargin

  def generateScript(): Unit = {
    val writer = new FileWriter(pathToScript)
    code foreach (writer.write(_))
    writer.close()
  }

  def execute(): String = {
    generateScript()
    Process("python", Seq(pathToScript)).!!
  }
}