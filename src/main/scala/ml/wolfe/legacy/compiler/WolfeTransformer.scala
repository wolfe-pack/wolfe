package ml.wolfe.legacy.compiler

import scala.tools.nsc.Global


/**
 * @author sameer
 */
trait WolfeTransformer {
  def transformTree[T <: Global#Tree](global: Global, env: WolfeCompilerPlugin2#Environment, tree: T): T
}

object WolfeTransformers extends WolfeTransformer {
  private val ops = scala.collection.mutable.ListBuffer[WolfeTransformer]()

  /**
   * Register a operator. This is wolfe private to ensure that bad things don't happen inadvertently
   *
   * @param operator
   */
  private[compiler] def register(operator: WolfeTransformer): Unit = {  ops += operator }

  def transformTree[T <: Global#Tree](global: Global, env: WolfeCompilerPlugin2#Environment, tree: T): T = {
    ops.foldLeft(tree) {
      (t: T, op: WolfeTransformer) => op.transformTree(global, env, tree)
    }
  }

}

/**
 * Prints the abstract syntax tree
 */
class DummyTransformer extends WolfeTransformer {
  override def transformTree[T <: Global#Tree](g2: Global, env: WolfeCompilerPlugin2#Environment, tree: T) = {
    println(tree)
    tree.asInstanceOf[T]
  }
}

object TransformerApp extends App {
  val dummyTransformer = new DummyTransformer
  val compiler = new StringCompiler(Some(dummyTransformer))

  val code =
    """
      | wolfe Wolfe
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

  compiler.compileCode(code)


}


