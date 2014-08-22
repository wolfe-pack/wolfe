import java.io.File

import ml.wolfe.ui.{Notebook, CodeBlock, HTMLFileRenderer}

val renderer = new HTMLFileRenderer(new File("/tmp/example.html"))

import renderer._
import Notebook._

object Code extends CodeBlock {
  val i = 1
  val j = 2
  val result = i + j
}
source(block(Code))
html(Code.result.toString)
h1("Yoo")

//source(CodeBlock) { html(result)}
md("#Example Header 2")

md { """
Some text.

## Another header

And a [link](http://www.google.com).

"""}



close()