package ml.wolfe.nlp

import ml.wolfe.WolfeSpec

/**
 * @author riedel
 */
class DocumentSpecs extends WolfeSpec {

  "A document" should {
    "be traversable" in {
      val pipeline = Document.fromString _ andThen SentenceSplitter andThen TokenSplitter
      val doc = pipeline("Sally met Harry. She was a girl. He was not.")

      import scala.pickling.Defaults._
      import scala.pickling.json._

      //println(doc.discourse.pickle)

    }
  }
}
