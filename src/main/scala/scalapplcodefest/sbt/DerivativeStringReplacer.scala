package scalapplcodefest.sbt

import scala.tools.nsc.Settings
import scala.reflect.internal.util.BatchSourceFile
import scala.reflect.io.{File, AbstractFile}

/**
 * Created by rockt on 07/01/2014.
 */
class DerivativeStringReplacer(val env:GeneratorEnvironment) extends CodeStringReplacer {
  var debug = false
  import env.global._

  def replace(tree: Tree, modification: ModifiedSourceText): Boolean = {

    if (debug && tree.toString().startsWith("scala.AnyRef {")) {
      treeBrowser.browse(tree)
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
    settings.stopAfter.value = List(SourceGeneratorCompilerPlugin.generationPhase)
    val source = new BatchSourceFile(AbstractFile.getFile(File("src/main/scala/scalapplcodefest/sbt/ExampleDifferentiableTemplate.scala")))
    SimpleCompiler.compile(settings, List(source),
      List(global => new SourceGeneratorCompilerPlugin(global, targetDir, List(new DerivativeStringReplacer(global)))))
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