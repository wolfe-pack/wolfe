package scalapplcodefest.compiler

import scala.tools.nsc.{ObjectRunner, Global}
import scala.reflect.io.{Path, AbstractFile, VirtualDirectory}


/**
 * User: rockt
 * Date: 1/2/14
 * Time: 2:17 PM
 */

object WolfePlayground extends App {
  val code = """
               |object A extends App {
               |  val x = 1
               |
               |  println(x)
               | }
             """.stripMargin


  val outputDir = AbstractFile.getDirectory(Path("tmp"))

  val compiler = new StringCompiler(Some(new PlaygroundTransformer), Nil, outputDir, runsAfter = List("typer"))


  compiler.compileCode(code)

  //ObjectRunner.run(List(compiler.outputDir.toURL), "A", "tmp" :: compiler.additionalClassPath ::: compiler.libPath ::: compiler.compilerPath)


  class PlaygroundTransformer extends WolfeTransformer {
    def transformTree[T <: Global#Tree](global: Global, env: WolfeCompilerPlugin2#Environment, tree: T): T = {
      import global._
      tree match {
        case v@ValDef(mods, name, tpt, r) => {
          println("Before: " + v)
          val newRHS = treeCopy.Literal(r, Constant(41))
          val newName = newTermName("newX")

          val newVal = treeCopy.ValDef(v, mods, newName, tpt, newRHS)
          println("After: " + newVal)
          newVal.asInstanceOf[T]
        }
        case t => t
      }
    }
  }
}
