package scalapplcodefest.compiler

import scala.tools.nsc.{Global, Settings}
import scala.tools.nsc.reporters.ConsoleReporter
import scala.reflect.io.VirtualDirectory

/**
 * @author sameer
 */
class StringCompiler(val transformer: Option[WolfeTransformer] = None) {
  val settings = new Settings
  settings.nowarnings.value = true // warnings are exceptions, so disable
  settings.outputDirs.setSingleOutput(new VirtualDirectory("(memory)", None))

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

  (libPath ::: compilerPath)
    .foreach {
    each =>
      settings.classpath.append(each)
      settings.bootclasspath.append(each)
  }

  val reporter = new ConsoleReporter(settings)

  def compileCode(code: String): (Global, CompilationUnit) = {
    val compiler : Global = if (transformer.isDefined) {
      new Global(settings, reporter) {
        self =>
        override protected def computeInternalPhases() {
          super.computeInternalPhases
          for (phase <- new WolfeCompilerPlugin(self, transformer.get).components)
            phasesSet += phase
        }
      }
    } else new Global(settings, reporter)
    val run = new compiler.Run

    val x1 = compiler.newUnitParser(code)

    x1.unit.body = x1.smartParse

    (compiler, x1.unit)
  }

  /*
   * For a given FQ classname, trick the resource finder into telling us the containing jar.
  */
  private def jarPathOfClass(className: String) = try {
    val resource = className.split('.').mkString("/", "/", ".class")
    val path = getClass.getResource(resource).getPath
    val indexOfFile = path.indexOf("file:") + 5
    val indexOfSeparator = path.lastIndexOf('!')
    List(path.substring(indexOfFile, indexOfSeparator))
  }
}

object StringCompiler extends StringCompiler(None)
