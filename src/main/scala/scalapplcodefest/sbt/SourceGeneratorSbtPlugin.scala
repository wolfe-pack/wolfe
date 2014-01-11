package scalapplcodefest.sbt

import sbt.{InputKey, Plugin}
import scala.tools.nsc.{Global, Settings}
import scala.reflect.internal.util.BatchSourceFile
import scala.reflect.io.{File, AbstractFile}
import scalapplcodefest.compiler

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

  def generate(sourcePath: String = "src/main/scala/scalapplcodefest/sbt/ExampleTemplate.scala",
               targetPath: String = "target/scala-2.10/sbt-0.13/src_managed/main/scala/",
               replacers: List[GeneratorEnvironment => CodeStringReplacer] = Nil) = {
    val targetDir = new java.io.File(targetPath)
    val settings = new Settings()
    settings.nowarnings.value = true
    settings.stopAfter.value = List(SourceGeneratorCompilerPlugin.generationPhase)
    settings.classpath.append(compiler.dirPathOfClass(getClass.getName))
    settings.bootclasspath.append(compiler.dirPathOfClass(getClass.getName))
    val source = new BatchSourceFile(AbstractFile.getFile(File(sourcePath)))
    SimpleCompiler.compile(
      settings,
      List(source),
      List(env => new SourceGeneratorCompilerPlugin(env, targetDir, replacers.map(_.apply(env)))))
  }

  def main(args: Array[String]) {
    generate()
  }

}




