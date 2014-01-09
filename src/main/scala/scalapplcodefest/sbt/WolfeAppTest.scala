package scalapplcodefest.sbt

import scala.tools.nsc.{Global, Settings}
import scalapplcodefest.compiler
import scala.reflect.internal.util.BatchSourceFile
import scala.reflect.io.{VirtualDirectory, AbstractFile}
import scala.tools.nsc.interpreter.AbstractFileClassLoader

/**
 * @author Sebastian Riedel
 */
object WolfeAppTest {

  /**
   * This method takes an object, searches for the source code of the object's class, generates
   * optimized code for this class, and then instantiates an object of the optimized class.
   * It hence returns an optimized version of the uncompiled input.
   */
  def compileAndCreate[T](uncompiled: Any, replacers: List[Global => CodeStringReplacer]): T = {
    //first generate optimized/replaced source code
    val packageName = uncompiled.getClass.getPackage.getName
    val className = uncompiled.getClass.getSimpleName
    val sourceDir = "src/main/scala/"
    val managedSourceDir = "target/scala-2.10/sbt-0.13/src_managed/main/scala/"
    val sourceFileName = uncompiled.getClass.getName.replaceAll("\\.", "/") + ".scala"
    val sourceFile = new java.io.File(sourceDir + sourceFileName)
    GenerateSources.generate(sourceFile.getAbsolutePath, managedSourceDir, replacers)

    //now compile and run the generated code
    val outputDir = new VirtualDirectory("(memory)", None)
    val compiledSourceFile = new java.io.File(s"$managedSourceDir${packageName.replaceAll("\\.", "/")}/compiled/$className.scala")
    val source = new BatchSourceFile(AbstractFile.getFile(compiledSourceFile))

    //compiler settings
    val settings = new Settings()
    settings.nowarnings.value = true
    settings.classpath.append(compiler.dirPathOfClass(getClass.getName))
    settings.bootclasspath.append(compiler.dirPathOfClass(getClass.getName))
    settings.outputDirs.setSingleOutput(outputDir)

    SimpleCompiler.compile(settings, List(source), Nil)

    //creating an instance of the generated class
    val classLoader = new AbstractFileClassLoader(outputDir, this.getClass.getClassLoader)
    val cls = classLoader.loadClass(s"$packageName.compiled.$className")
    val instance = cls.newInstance()
    instance.asInstanceOf[T]
  }

  def compareCompiled() {
    val uncompiled = new CoinTossingToBeCompiled
    val compiled = compileAndCreate[() => Any](uncompiled, List(new MLECodeReplacer(_)))
    println(uncompiled())
    println(compiled())

  }

  def main(args: Array[String]) {
    compareCompiled()
  }

}

trait WolfeApp {
  def apply(): Any
}