package scalapplcodefest.compiler

import scala.reflect.internal.Trees
import scalapplcodefest.compiler._
import scala.tools.nsc.Global

/**
 * @author sameer
 */
trait WolfeTransformer {
  def transform[T <: CompilationUnit](unit: T, global: Global): Unit
}

/**
 * Prints the abstract syntax tree
 */
class DummyTransformer extends WolfeTransformer {
  def transform[T <: CompilationUnit](unit: T, global: Global) = {
    unit.body match {
      case _ => println("Can't compile")
    }
  }
}

object TransformerApp extends App {
  val compiler = new StringCompiler

  val code =
    """
      | package Wolfe
      | object Foo {
      |   val a = 1
      |   val b = 2
      |
      |   def add(x: Int, y: Int) = x + y
      |
      |   add(a, b)
      | }
    """.stripMargin

  val (tree, global) = compiler.compileCode(code)

  val transformer = new DummyTransformer
  transformer.transform(tree, global)
}


