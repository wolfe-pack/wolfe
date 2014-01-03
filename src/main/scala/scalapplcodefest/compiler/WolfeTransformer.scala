package scalapplcodefest.compiler

import scala.tools.nsc.Global

/**
 * @author sameer
 */
trait WolfeTransformer {
  def transform(global: Global)(unit: global.type#CompilationUnit): Unit
}

/**
 * Prints the abstract syntax tree
 */
class DummyTransformer extends WolfeTransformer {

  def transform(global: Global)(unit: global.type#CompilationUnit): Unit = {
    for (tree <- unit.body) {
      transformTree(global)(tree)
    }

    //for debugging
    //global.treeBrowser.browse(unit.body)

    println()
    println("after transformation:")
    println()

    transformTest(global)(unit)
  }

  def transformTest(global: Global)(unit: global.type#CompilationUnit): Unit = {
    //testing AST transformer template
    val transformed = TreeOperations.noopTemplate(global)(unit.body)
    for (tree <- transformed) {
      transformTree(global)(tree)
    }
  }

  def transformTree(global: Global)(tree: global.Tree): Unit = {
    import global._

    tree match {
        //example Types
      case Ident(typeName) => println(s"basic type $typeName")
        //example Aliasing new types
      case TypeDef(_, from, _, to) => println(s"defined $from as $to")
        //example Creating objects
      case Literal(Constant(c)) => println(s"constant $c")
        //example Assignments to val
      case ValDef(_, name, _, rhs) => println(s"assigned $rhs to $name")
        //example Function calls
      case Apply(fun, args) => println(s"applying $fun to $args")
        //example Defining functions
      case DefDef(_, name, tparams, vparamss, _, rhs) => println(s"defining $name($tparams) as $rhs")
      case PackageDef(pid, _) => println("found PackageDef: " + pid.name)
      case ClassDef(_, name, _, _) => println("found ClassDef: " + name)
      case ModuleDef(_, name, _) => println("found ModuleDef: " + name)


      //case TypeDef(_, name, _, _) => println("found TypeDef: " + name)
      case Literal(value) => println("found Literal: " + value)
      case _ => //println("Can't compile: " + tree)
    }
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


