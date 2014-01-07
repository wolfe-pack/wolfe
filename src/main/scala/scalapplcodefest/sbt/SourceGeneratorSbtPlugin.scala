package scalapplcodefest.sbt

import sbt.{InputKey, Plugin}
import scala.tools.nsc.Settings
import scala.reflect.internal.util.BatchSourceFile
import scala.reflect.io.{File, AbstractFile}

/**
 * @author Sebastian Riedel
 */
class SourceGeneratorSbtPlugin extends Plugin {

  lazy val generateSource = InputKey[Unit]("generate source")

  generateSource := {
    println("Yeah")
  }

}

object GenerateSources {

  def generate() = {
    val settings = new Settings()
    settings.nowarnings.value = true
    settings.stopAfter.value = List(SourceGeneratorCompilerPlugin.phase)
    val source = new BatchSourceFile(AbstractFile.getFile(File("src/main/scala/scalapplcodefest/sbt/ExampleTemplate.scala")))
    SimpleCompiler.compile(settings,List(source),List(new SourceGeneratorCompilerPlugin(_)))
  }

  def main(args: Array[String]) {
    generate()
  }

}




