package scalapplcodefest.compiler

import scala.tools.nsc.Global

/**
 * @author sameer
 */
trait WolfeTransformer {
  def transformTree[T <: Global#Tree](global: Global, tree: T): T
}

/**
 * Prints the abstract syntax tree
 */
class DummyTransformer extends WolfeTransformer {
  override def transformTree[T <: Global#Tree](g2: Global, tree: T) = {
    println(tree)
    tree.asInstanceOf[T]
  }
}

object TransformerApp extends App {
  val dummyTransformer = new DummyTransformer
  val compiler = new StringCompiler(Some(dummyTransformer))

  val code =
    """
      | package Wolfe
      | object Foo {
      |   type Foo1 = Int
      |
      |   val a: Int = 1
      |   val b: Double = 2
      |
      |   def add(x: Int, y: Double) = x + y
      |
      |   add(a, b)
      | }
    """.stripMargin

  val (global, unit) = compiler.compileCode(code)


}


