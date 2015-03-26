package ml.wolfe.nlp

import ml.wolfe.WolfeSpec

/**
 * @author riedel
 */
class NavigableDocumentSpecs extends WolfeSpec {

  "A navigable document" should {
    "allow back traversal" in {
      val pipeline = Document.fromString _ andThen SentenceSplitter andThen TokenSplitter
      val doc = pipeline("Sally met Harry. She was a girl. He was not.")
      val nav = new NavigableDocument(doc)
      import nav._
      doc.sentences(1).tokens(2).sentence should be (doc.sentences(1))
      doc.sentences(1).tokens(2).offsets.tokens should be (Vector(doc.sentences(1).tokens(2)))
      doc.sentences(1).document should be (doc)
      doc.sentences(1).tokens(2).index should be (2)

    }
  }
}
