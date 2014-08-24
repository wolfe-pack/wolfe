package ml.wolfe.ui

import java.io.File

import ml.wolfe.Wolfe

/**
 * @author Sebastian Riedel
 */
object ExampleNotebook extends MutableHTMLNotebook with App {

  import ml.wolfe.ui.RevealJS._
  import Wolfe._
  import ml.wolfe.macros.OptimizedOperators._
  import ml.wolfe.D3Implicits._

  h1( "This is a Notebook")

  case class Sample(x:Boolean,y:Boolean)
  @OutputFactorGraph
  def model(s:Sample) = I(s.x == s.y) + I(s.x)
  val result = argmax(all(Sample)) {model}

  html(d3fg(FactorGraphBuffer.get()).source)

  //startCode()
  //val i = 1
  //val j = 2
  //i + j
  //endCode()
  //source(lastCode)
  //source(lastResult)
  //editedSVG("myfigure")

  md("""
     ## Header
     Blah blah *cursive* blub.

    """)

  slides {
    slide {
      h1("This is a slide show")
    }
    slide {
      h2("This is a slide")
    }
  }

  saveTo(new File("/tmp/example2.html"), new SimpleTemplate())

}
