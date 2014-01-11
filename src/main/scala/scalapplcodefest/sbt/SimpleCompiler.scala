package scalapplcodefest.sbt

import scala.tools.nsc.{Global, Settings}
import scala.reflect.internal.util.{SourceFile, BatchSourceFile}
import scala.tools.nsc.reporters.{ConsoleReporter, AbstractReporter}
import scala.tools.nsc.interactive.RangePositions

/**
 * String compiler that requires a little more work for the client, but has more flexibility.
 */
object SimpleCompiler {

  import scalapplcodefest.compiler.CompilerHelpers._

  /**
   * Adds reasonable default classpath
   * @param settings settings to change.
   */
  def setDefaultClasspath(settings: Settings) {
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
  }

  def inlineFile(code:String) = new BatchSourceFile("(inline)", code)

  def compile(settings: Settings, sources:List[SourceFile], pluginConstructors: List[GeneratorEnvironment => scala.tools.nsc.plugins.Plugin] = Nil) {
    val reporter = new ConsoleReporter(settings)
    setDefaultClasspath(settings)
    val compiler: Global =
      new Global(settings, reporter) with RangePositions {
        self =>
        override protected def computeInternalPhases() {
          super.computeInternalPhases()
          val env = new GeneratorEnvironment(self)

          for (constructor <- pluginConstructors; phase <- constructor(env).components)
            phasesSet += phase
        }
      }

    val run = new compiler.Run
    run.compileSources(sources)
  }

}
