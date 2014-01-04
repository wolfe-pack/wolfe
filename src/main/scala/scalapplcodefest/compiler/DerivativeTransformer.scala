package scalapplcodefest.compiler

import scala.tools.nsc.Global
import java.io._
import scala.Console

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
  val function = "x**3 + 4 * x**2 + 1" //TODO: we can have more than one symbol x
  println(s"f: $function")

  val sympy = SymPyDerivator(symbol, function)

  val derivative = sympy.execute()

  println(s"f d$symbol: $derivative")
  //build AST

  //TODO
}

case class SymPyDerivator(symbol: String, function: String) {
  import scala.sys.process.Process

  val pathToScript = "/tmp/test.py"

  val code =
  s"""
    |from sympy import *
    |import numpy as np
    |
    |x = Symbol("$symbol")
    |f = $function
    |
    |fprime = f.diff(x)
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