package ml.wolfe.legacy.compiler

import scala.tools.nsc.{Global, Settings}
import scala.tools.nsc.reporters.ConsoleReporter
import scala.reflect.io.{AbstractFile, VirtualDirectory}
import scala.reflect.internal.util.BatchSourceFile

/**
 * @author sameer
 */
class StringCompiler(val transformer: Option[WolfeTransformer] = None,
                     val additionalClassPath: List[String] = Nil,
                     val outputDir: AbstractFile = new VirtualDirectory("(memory)", None),
                     runsAfter: List[String] = List("refchecks"),
                     val foreignGlobal: Option[Global] = None) {
  val settings = new Settings
  settings.nowarnings.value = true // warnings are exceptions, so disable
  settings.outputDirs.setSingleOutput(outputDir)

  import CompilerHelpers._

  val compilerPath = try {
    jarPathOfClass("scala.tools.nsc.Interpreter")
  } catch {
    case e: Throwable =>
      throw new RuntimeException("Unable lo load scala interpreter from classpath (scala-compiler jar is missing?)", e)
  }

  val libPath = try {
    jarPathOfClass("scala.ScalaObject")
  } catch {
    case e: Throwable =>
      throw new RuntimeException("Unable to load scala base object from classpath (scala-library jar is missing?)", e)
  }

  (libPath ::: compilerPath ::: additionalClassPath)
    .foreach {
    each =>
      settings.classpath.append(each)
      settings.bootclasspath.append(each)
  }

  val reporter = new ConsoleReporter(settings)

  def compileCode(code: String) = {
    val compiler: Global =
      if (foreignGlobal.isDefined) foreignGlobal.get
      else {
        if (transformer.isDefined) {
          new Global(settings, reporter) {
            self =>
            override protected def computeInternalPhases() {
              super.computeInternalPhases

              for (phase <- new WolfeCompilerPlugin2(self, transformer.get, runsAfter).components)
                phasesSet += phase
            }
          }
        } else new Global(settings, reporter)
      }

    val run = new compiler.Run

    run.compileSources(List(new BatchSourceFile("(inline)", code)))

    val x1 = compiler.newUnitParser(code)
    x1.unit.body = x1.smartParse


    (compiler, x1.unit)
  }


}

object CompilerHelpers {
  /*
    * For a given FQ classname, trick the resource finder into telling us the containing jar.
   */
  def jarPathOfClass(className: String) = try {
    val resource = className.split('.').mkString("/", "/", ".class")
    val path = getClass.getResource(resource).getPath
    val indexOfFile = path.indexOf("file:") + 5
    val indexOfSeparator = path.lastIndexOf('!')
    List(path.substring(indexOfFile, indexOfSeparator))
  }

}

object StringCompiler extends StringCompiler(None, Nil, new VirtualDirectory("(memory)", None),List("refchecks"), None)
