package scalapplcodefest.sbt

import scala.tools.nsc.{Global, Settings}
import scala.reflect.internal.util.BatchSourceFile
import scala.reflect.io.{File, AbstractFile}

/**
 * Created by rockt on 07/01/2014.
 */
class DerivativeStringReplacer extends CodeStringReplacer {
  var debug = false

  def replace(global: Global)(tree: global.type#Tree, modification: ModifiedSourceText): Boolean = {
    import global._

    if (debug && tree.toString().startsWith("scala.AnyRef {")) {
      global.treeBrowser.browse(tree)
      debug = false
    }

   tree match {
      case fun @ DefDef(_, _, _, _, _, rhs) => {
        if (fun.name.decoded == "sigmoid")
          modification.insert(rhs.pos.end,
            "\n  @Compiled(\"scalapplcodefest.sbt.ExampleDifferentiableTemplate.sigmoid\") " +
              "def sigmoidDiff(z: Double) = math.exp(-z) / math.pow(1 + math.exp(-z), 2)"
          )
      }
      case _ => //println("processing other: " + tree)//ignore
    }

    true
  }
}


object GeneratedDifferentialSourcesPlayground extends App {
  def generate() = {
    val targetDir = new java.io.File("target/scala-2.10/sbt-0.13/src_managed/main/scala/")
    val settings = new Settings()
    settings.nowarnings.value = true
    settings.stopAfter.value = List(SourceGeneratorCompilerPlugin.phase)
    val source = new BatchSourceFile(AbstractFile.getFile(File("src/main/scala/scalapplcodefest/sbt/ExampleDifferentiableTemplate.scala")))
    SimpleCompiler.compile(settings, List(source),
      List(new SourceGeneratorCompilerPlugin(_, targetDir, List(new DerivativeStringReplacer))))
  }

  generate()
}

object TestGeneratedSource extends App {
  val z = 2.0

  println(
    s"""
      |z:           $z
      |sigmoid(z):  ${ExampleDifferentiableTemplate.sigmoid(z)}
    """.stripMargin)
}