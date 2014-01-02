package scalapplcodefest.compiler


/**
 * User: rockt
 * Date: 1/2/14
 * Time: 2:17 PM
 */

object WolfePlayground extends App {
  val code = """
               |object A {
               |  val x = 1
               | }
             """.stripMargin

  val (global, unit) = StringCompiler.compileCode(code)

  val tree = unit.body.asInstanceOf[global.Tree]

  global.treeBrowser.browse(tree)

}