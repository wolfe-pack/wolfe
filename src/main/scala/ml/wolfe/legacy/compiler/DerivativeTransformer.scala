package ml.wolfe.legacy.compiler

import scala.tools.nsc.Global
import java.io.FileWriter
import ml.wolfe.Wolfe.Objective._
import scala.Some

/**
 * User: rockt
 * Date: 1/4/14
 * Time: 10:27 AM
 */

//example code: http://lampsvn.epfl.ch/svn-repos/scala/scala/branches/scala-unique/src/unique/plugin/SwapTransform.scala

/**
 * Searches for @Objective.Differentiable annotation and generates an AST for gradient calculation
 */
class DerivativeTransformer extends WolfeTransformer {                   
  import SymPyDSL._
  
  def checkDifferentiatorType(tpe: String, diff: Differentiator): Boolean =
    tpe.startsWith("Some["+ diff.getClass.getName.replace('$','.'))
  
  val Diff = new Differentiable

  def transformTree[T <: Global#Tree](global: Global, env: WolfeCompilerPlugin2#Environment, tree: T) = {
    import global._

    tree match {
      //we are interested in function definitions with annotations
      case fun @ DefDef(a, b, c, d, e, rhs) if existsAnnotation(fun, Diff) =>
        val annotationInfo = fun.symbol.annotations.find(a => sameAnnotation(a, Diff)).get

        if (checkDifferentiatorType(annotationInfo.args(1).tpe.toString(), Wolferine)) {
          treeCopy.DefDef(tree.asInstanceOf[global.Tree], a, b, c, d, e,
            TheWolferine(global, env, rhs)
          ).asInstanceOf[T]
        } else if (checkDifferentiatorType(annotationInfo.args(1).tpe.toString(), SymPy)) {
          val python = pythify(rhs.toString())
          println(
            s"""
              |===== Differentiating '${fun.name}' using SymPy =====
              | scala objective:     $rhs
              | python objective:    $python
              | python differential: ${python.differential()}
          """.stripMargin)
          tree //TODO: replace rhs with scala function that represents the differential
        } else {
          tree //nothing to do
        }
      case _ => tree
    }
  }

  //rockt: this should be generalized (e.g. for toLaTeX, toPython etc.)
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
    .replace("scala.math.`wolfe`.", "")
    .replace(".unary_-", "*(-1)")
    .replace(".", "")
}

object TheWolferine {
  def apply[T <: Global#Tree](global: Global, env: WolfeCompilerPlugin2#Environment, tree: T): T = {
    import global._
    import definitions._
    import typer.{typed}

    tree match {
      case Apply(Select(arg @ Ident(a), method), List(Ident(b)))
        if method.decoded == "*" && a.decoded == b.decoded =>
        typed(atPos(tree.pos)(
          Apply(Select(Literal(Constant(2)), method), List(arg))
        )).asInstanceOf[T]
      case _ => {
        println(s"Wolferine does not know yet how to differentiate: $tree")
        tree
      }
    }
  }
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

  val Imports =
    """
      |  import math._
      |  import ml.wolfe.Wolfe.Output
      |  import ml.wolfe.Wolfe.Objective
      |  import ml.wolfe.Wolfe.Objective.Wolferine
      |  import ml.wolfe.Wolfe.Objective.SymPy
    """.stripMargin
  
  val sigmoidCode = Imports +
    """
      |  @Objective.Differentiable(differentiator = Some(SymPy))
      |  @Output.LaTeX
      |  def sigmoid(z: Double) = 1 / (1 + exp(-z))
    """.stripMargin

  val simpleCode = Imports +
    """
      |  @Objective.Differentiable(differentiator = Some(Wolferine))
      |  @Output.LaTeX
      |  def simple(z: Double) = z * z + z
    """.stripMargin

  val additionalClassPath = List(dirPathOfClass(getClass.getName))

  ASTExplorer.exploreAST(simpleCode, List("typer"), showBrowser = true,
    additionalClassPath = additionalClassPath, transformers = List(new DerivativeTransformer))
}