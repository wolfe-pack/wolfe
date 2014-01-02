package scalapplcodefest.compiler

import scala.tools.nsc.{Global, Settings}
import scala.tools.nsc.reporters.ConsoleReporter

/**
 * User: rockt
 * Date: 1/2/14
 * Time: 2:17 PM
 */

object WolfePlayground extends App {

  val settings = new Settings
  settings.outdir.value = "/tmp"

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

  /**
   * For a given FQ classname, trick the resource finder into telling us the containing jar.
   */
  private def jarPathOfClass(className: String) = try {
    val resource = className.split('.').mkString("/", "/", ".class")
    val path = getClass.getResource(resource).getPath
    val indexOfFile = path.indexOf("file:") + 5
    val indexOfSeparator = path.lastIndexOf('!')
    List(path.substring(indexOfFile, indexOfSeparator))
  }


  (
    libPath ::: compilerPath ::: List("./lib/scala-2.10.3/scala-reflect.jar")
    ).foreach {
    each =>
      settings.classpath.append(each)
      settings.bootclasspath.append(each)

  }

  val reporter = new ConsoleReporter(settings)
  val compiler = new Global(settings, reporter)
  val run = new compiler.Run

  val x1 = compiler.newUnitParser(
    """
      |object A {
      | type Person = String
      | }
    """.stripMargin)

  val x1Parsed = x1.smartParse()

  compiler.treeBrowser.browse(x1Parsed)

  def f(x: Int) = x + 1

}