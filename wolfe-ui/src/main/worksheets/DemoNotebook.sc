import java.io.File

import ml.wolfe.ui._

implicit val renderer = new MutableHTMLNotebook()


import renderer._
import Notebook._
import RevealJS._

object Test {
  val i  = 2
  val j  = 3
  i + j
  val actual = lastNStatements(3, 1)
}
//println(Test.actual)
source(Test.actual)
//source(actual) flip()
h1("Yoo")
//source(CodeBlock) { html(result)}
md("#Example Header 2")
md { """
Some text.
## Another header     blah

And a [link](http://www.google.com).

     """
}
slides {
  slide {
    h1("Slide header")
  }
}
saveTo(new File("/tmp/example.html"), new SimpleTemplate())