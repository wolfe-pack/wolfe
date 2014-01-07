package scalapplcodefest.sbt

import scala.tools.nsc.{Global, Settings}
import scala.reflect.internal.util.BatchSourceFile
import scala.reflect.io.{File, AbstractFile}

/**
 * Created by rockt on 07/01/2014.
 */
class DerivativeStringReplacer extends CodeStringReplacer {
  def replace(global: Global)(tree: global.type#Tree, modification: ModifiedSourceText): Boolean = {
    import global._

    tree match {
      case DefDef(_, _, _, _, _, rhs) => println("processing DefDef ...")
      case _ => println("processing rest:\n" + tree)
    }

    if (tree.toString() == "\"Test\"")
      modification.replace(tree.pos.start, tree.pos.end, "\"FooTest!\"")

    true
  }
}


object GenerateSourcesPlayground extends App {
  def generate() = {
    val targetDir = new java.io.File("target/scala-2.10/sbt-0.13/src_managed/main/scala/")
    val settings = new Settings()
    settings.nowarnings.value = true
    settings.stopAfter.value = List(SourceGeneratorCompilerPlugin.phase)
    val source = new BatchSourceFile(AbstractFile.getFile(File("src/main/scala/scalapplcodefest/sbt/ExampleTemplate.scala")))
    SimpleCompiler.compile(settings, List(source),
      List(new SourceGeneratorCompilerPlugin(_, targetDir, List(new DerivativeStringReplacer))))
  }

  generate()
}