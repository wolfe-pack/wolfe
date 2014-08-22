package ml.wolfe.ui

import java.io.{File, PrintStream}

import eu.henkelmann.actuarius.ActuariusTransformer

import scala.reflect.macros.Context

/**
 * @author Sebastian Riedel
 */
trait NotebookRenderer {

  val transformer = new ActuariusTransformer

  def html(text: String)

  def md(markdown: String): Unit = {
    html(transformer(markdown))
  }
  def h1(text: String) = html(s"<h1>$text</h1>")
  def h2(text: String) = html(s"<h1>$text</h1>")
  def source(src: String) = html(s"<pre>$src</pre>")

  def close()

}

class HTMLFileRenderer(dst: File, template: String => String = SimpleTemplate) extends NotebookRenderer {

  dst.getParentFile.mkdirs()

  val tmp     = File.createTempFile(dst.getName, "html")
  val builder = new StringBuilder
  val out     = new PrintStream(tmp)

  def html(text: String) = builder append text
  def close() = {
    out.println(template(builder.toString()))
    out.close()
    tmp.renameTo(dst)
  }
}

object SimpleTemplate extends (String => String) {

  def apply(content: String) = {
    s"""
        |<!DOCTYPE html>
        |<html lang="en">
        |
        |<head>
        |    <script type="text/javascript">
        |        var last = ""
        |        setInterval(function () {
        |                var href = document.URL;
        |                var xmlhttp = new XMLHttpRequest();
        |                xmlhttp.open("GET", href, false);
        |                xmlhttp.send();
        |                var changed = last != "" && last != xmlhttp.responseText;
        |                //console.log("Changed: " + changed);
        |                if (changed) {
        |                    window.location = href;
        |                }
        |                last = xmlhttp.responseText;
        |            },
        |            100);
        |    </script>
        |</head>
        |
        |<body>
        |$content
        |</body>
        </html>
    """.stripMargin
  }
}

trait CodeBlock {
  val result: Any
}

object Notebook {

  import scala.language.experimental.macros

  def block(code: CodeBlock): String = macro blockImpl

  def blockImpl(c: Context)(code: c.Expr[CodeBlock]) = {
    import c.universe._
    val blockSymbol = code.tree.symbol
    val parents = c.enclosingUnit.body.collect({
      case parent => parent.children map (child => child -> parent)
    }).flatMap(identity).toMap
    def next(tree: Tree, offset: Int = 1): Option[Tree] = parents.get(tree) match {
      case Some(parent) =>
        val children = parent.children.toIndexedSeq
        val index = children.indexOf(tree)
        children.lift(index + offset) match {
          case Some(sibling) => Some(sibling)
          case _ => None
        }
      case _ => None
    }

    val moduleDefs = c.enclosingUnit.body.collect({
      case mdef: ModuleDef if mdef.symbol == blockSymbol => mdef
    })
    val sourceFile = c.enclosingUnit.source
    def startOfLine(point: Int) = sourceFile.lineToOffset(sourceFile.offsetToLine(point))

    //todo need to use original source file to recover the source code, as we otherwise drop comments, empty lines etc.
    val source = moduleDefs match {
      case blockDef :: Nil =>
        val firstNonInit = blockDef.impl.body(1)
        val last = blockDef.impl.body.last
        val txt = sourceFile.content.subSequence(startOfLine(firstNonInit.pos.point), startOfLine(last.pos.point)).toString
        val below = next(blockDef, +1).get

        val lastLine = last match {
          case ValDef(_, _, _, rhs) =>
            //            val start = rhs.pos.point
            //            val end = sourceFile.lineToOffset(sourceFile.offsetToLine(start) + 1)
            //            sourceFile.content.subSequence(start - 7,end).toString
            val result = last.pos.lineContent.replaceAll("val result = ", "")
            //show(rhs)
            result
          case _ => c.abort(c.enclosingPosition, "Bad")
        }
        val lines = (txt.split("\n") :+ lastLine) map (_.trim) mkString "\n"

        //
        //        val beginningOfLine = sourceFile.lineToOffset(sourceFile.offsetToLine(below.pos.point))
        //        val txt = sourceFile.content.subSequence(blockDef.pos.point,beginningOfLine).toString
        //        show(txt)
        //        val lines = for (line <- blockDef.impl.body.drop(1)) yield {
        //          show(line)
        //        }
        //        lines mkString "\n"
        lines
      case _ => c.abort(c.enclosingPosition, "Can't find a definition of a CodeBlock object corresponding to " + code)
    }

    c.literal(source)
  }


}