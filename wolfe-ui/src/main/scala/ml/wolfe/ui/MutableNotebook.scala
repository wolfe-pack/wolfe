package ml.wolfe.ui

import java.io.{File, PrintStream}

import eu.henkelmann.actuarius.ActuariusTransformer
import org.sameersingh.htmlgen.HTML

import scala.reflect.macros.Context

/**
 * @author Sebastian Riedel
 */
trait MutableNotebook[T <: MutableNotebook[T]] {

  this:T =>

  val transformer = new ActuariusTransformer

  def html(text: String):T

  def html(h:HTML):T = html(h.source)

  def md(markdown: String): T = {
    html(transformer(markdown))
  }
  def h1(text: String) = html(s"<h1>$text</h1>")
  def h2(text: String) = html(s"<h2>$text</h2>")
  def source(src: String) = html(s"<pre>$src</pre>")

}

class MutableHTMLNotebook extends MutableNotebook[MutableHTMLNotebook] {

  implicit val renderer = this


  var builder:List[String] = Nil

  def html(text: String) = {
    builder ::= text
    this
  }

  def saveTo(dst: File,template: String => String = new SimpleTemplate): Unit = {
    val tmp = File.createTempFile(dst.getName, "html")
    val out = new PrintStream(tmp)
    out.println(template(builder.reverse mkString "\n"))
    out.close()
    dst.getParentFile.mkdirs()

    tmp.renameTo(dst)
  }

  def flip() = {
    builder = builder match {
      case l1 :: l2 :: t => l2 :: l1 :: t
      case l => l
    }
  }
}

class SimpleTemplate(head:String = "") extends (String => String) {

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
        |    $head
        |</head>
        |
        |<body>
        |$content
        |</body>
        </html>
    """.stripMargin
  }
}

object RevealJS {
  def slides(body: => Unit)(implicit renderer: MutableNotebook[_]): Unit = {
    renderer.html( """<div class="reveal"><div class="slides">""")
    body
    renderer.html( """</div></div>""")
  }
  def slide(body: => Unit)(implicit renderer: MutableNotebook[_]): Unit = {
    renderer.html( """<section>""")
    body
    renderer.html( """</section""")
  }
}


trait CodeBlock {
  val result: Any
}

object Notebook {

  import scala.language.experimental.macros

  def lastNStatements(n:Int, level:Int = 1):String = macro lastNStatementsImpl

  def parentMap[C<:Context](c:C)(tree:c.Tree):Map[c.Tree,c.Tree] = {
    import c.universe._
    val parents = tree.collect({
      case parent =>
        parent.children map (child => child -> parent)
    }).flatMap(identity).toMap
    parents
  }

  def lastNStatementsImpl(c:Context)(n:c.Expr[Int],level:c.Expr[Int]) = {
    import c.universe._
    //println(c.enclosingUnit.source.content.mkString(""))
    def offset(tree:Tree) = tree match {
      case v:ValDef => v.pos.point - 4
      case _ => tree.pos.point
    }
    def visibleChildren(tree:Tree) = tree match {
      case t:Template => t.children.drop(3)
      case _ => tree.children
    }
    val exprRaw = c.macroApplication
    val expr = c.enclosingUnit.body.find(exprRaw.pos.point == _.pos.point).get
    val parents = parentMap[c.type](c)(c.enclosingUnit.body)
    val constN = n.tree match {
      case Literal(constant) => constant.value.asInstanceOf[Int]
      case _ => c.abort(c.enclosingPosition,"N must be a literal integer")
    }
    val constLevel = level.tree match {
      case Literal(const) => const.value.asInstanceOf[Int]
      case _ => c.abort(c.enclosingPosition,"Level must be a literal integer")
    }
   def findAncestor(current:Tree, levelsLeft:Int): Tree = levelsLeft match {
      case 0 => current
      case _ => findAncestor(parents(current),levelsLeft - 1)
    }
    val ancestor = findAncestor(expr,constLevel)
    val parent = parents(ancestor)
    val children = visibleChildren(parent).toIndexedSeq
    val index = children.indexOf(ancestor)
    val beginIndex = math.max(0, index - (constN + 2))
    val beginOffset = offset(children(beginIndex))
    val endOffset = offset(ancestor)
    val source = c.enclosingUnit.source.content.subSequence(beginOffset,endOffset).toString
    val trimmed = source split "\n" map (_.trim) mkString "\n"
    c.literal(trimmed)
  }



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