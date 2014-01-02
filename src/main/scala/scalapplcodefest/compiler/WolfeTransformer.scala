package scalapplcodefest.compiler

/**
 * @author sameer
 */
trait WolfeTransformer {
  def transform[T <: CompilationUnit](unit: T): T
}

/**
 * Prints the abstract syntax tree
 */
class DummyTransformer extends WolfeTransformer {
  def transform[Tree](tree: Tree): Tree = {
    tree match {
      case ??? => println(tree)
      case _ => println("Can't compile")
    }

    tree
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


