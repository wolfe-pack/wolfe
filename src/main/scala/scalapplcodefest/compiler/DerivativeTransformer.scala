package scalapplcodefest.compiler

import scala.tools.nsc.Global
import java.io.{PrintStream, ByteArrayOutputStream, FileWriter}

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
  val function = "x ** 2 + 1"
  println(s"f: $function")

  val sympy = SymPyDerivator(symbol, function)

  val derivative = sympy.execute()

  println(s"f d$symbol: $derivative")
  //build AST

  //TODO
}

case class SymPyDerivator(symbol: String, function: String) {
  import scala.sys.process.Process

  val pathToScript = "/home/trocktae/test.py"

  val program =
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
    program foreach (writer.write(_))
    writer.close()
  }

  def execute(): String = {
    generateScript()
    val baos = new ByteArrayOutputStream()
    val ps = new PrintStream(baos)

    val tmpOut = Console.out
    Console.setOut(ps)
    Process("python", Seq(pathToScript)).!
    Console.setOut(tmpOut)
    
    baos.toString
  }
}