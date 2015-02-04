package ml.wolfe.ui

import java.io.File

import ml.wolfe.Wolfe

/**
 * @author Sebastian Riedel
 */
object ExampleSlideshow extends MutableHTMLNotebook with App {

  import ml.wolfe.Wolfe._
  import ml.wolfe.ui.RevealJS._

//  h1( "This is a Notebook")
//
//  case class Sample(x:Boolean,y:Boolean)
//  @OutputFactorGraph
//  def model(s:Sample) = I(s.x == s.y) + I(s.x)
//  val result = argmax(all(Sample)) {model}
//
//  html(d3fg(FactorGraphBuffer.get()).source)
//
//  //startCode()
//  //val i = 1
//  //val j = 2
//  //i + j
//  //endCode()
//  //source(lastCode)
//  //source(lastResult)
//  //editedSVG("myfigure")
//
//  md("""
//     ## Header
//     Blah blah *cursive* blub.
//
//    """)

//  slides {
//    slide {
//      h1("This is a slide show")
//      md("Yoo!")
//    }
//    slide {
//      h2("This is a slide")
//      md("Whaaat?")
//      case class Sample(x: Boolean, y: Boolean)
//      @OutputFactorGraph
//      def model(s: Sample) = I(s.x == s.y) + I(s.x)
//      val result = argmax(all(Sample)) { model }
//
//      html(d3fg(FactorGraphBuffer.get()).source)
//    }
//    slide {
//      h2("This is another slide")
//      md("Okay!")
//    }
//
//
//  }

  saveTo(new File("/Users/sriedel/projects/wolfe/wolfe-ui/web/reveal.js-2.6.2/notebook.html"), new SimpleTemplate)

}
