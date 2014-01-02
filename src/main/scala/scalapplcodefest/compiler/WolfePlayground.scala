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

  List[String](
    "./lib/scala-2.10.3/scala-library.jar",
    "./lib/scala-2.10.3/scala-compiler.jar",
    "./lib/scala-2.10.3/scala-reflect.jar"
  ).foreach { each =>
    settings.classpath.append(each)
    settings.bootclasspath.append(each)

  }

  val reporter = new ConsoleReporter(settings)
  val compiler = new Global(settings, reporter)
  val run = new compiler.Run

  val x1 = compiler.newUnitParser(
    """
      | val x = 1
    """.stripMargin)

  val x1parsed = x1.smartParse()

  x1parsed.foreach { t =>
    t.shortClass match {
      case "PackageDef" =>
        println("PackageDef")
        t.foreach { p =>
          p.shortClass match {
            case "Select" => {
              println("Select:" + p)
            }
            case _ =>
          }
        }
      case "ClassDef" =>
        println("ClassDef:" + t)
        t.foreach { c =>
          println(c.shortClass)
          println("\n")
       }
      case _ =>
    }
  }
}