package scalapplcodefest.compiler

import scala.reflect.internal.Trees
import scalapplcodefest.compiler._
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
    unit.body match {
      case global.PackageDef(pid, stats) => println("found PackageDef " + pid.name)
      case global.ModuleDef(mods, name, impl) => println("found Module: " + name)
      case global.TypeDef(_, name, _, rhs) => println(name)
      case _ => println("Can't compile")
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
      |   val a = 1
      |   val b = 2
      |
      |   def add(x: Int, y: Int) = x + y
      |
      |   add(a, b)
      | }
    """.stripMargin

  val (global, unit) = compiler.compileCode(code)
}


