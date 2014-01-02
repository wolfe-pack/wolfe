package scalapplcodefest.compiler

import scala.reflect.internal.Trees
import scalapplcodefest.compiler._

/**
 * @author sameer
 */
trait WolfeTransformer {
  def transform[T <: CompilationUnit](unit: T): Unit
}

/**
 * Prints the abstract syntax tree
 */
class DummyTransformer extends WolfeTransformer {
  def transform[T <: CompilationUnit](unit: T) = {
    unit.body match {
      case ???  => ???
      case _ => println("Can't compile")
    }

    unit
  }
}

object TransformerApp extends App {
  val compiler = new StringCompiler

  val code =
    """
      | object A {
      |   val x = 1
      | }
    """.stripMargin

  val tree = compiler.compileCode(code)

  val transformer = new DummyTransformer
  transformer.transform(tree)
}


