package ml.wolfe.nlp

import ml.wolfe.{nlp, WolfeSpec}

/**
 * @author Sebastian Riedel
 */
class SplitterSpecs extends WolfeSpec {

  import nlp._

  "A Token Splitter" should {
    "Split whitespace" in {
      val actual = normalizeDoc(TokenSplitter("This is text. Can you split it?"))
      val expected = makeDoc(Seq(Seq("This", "is", "text.", "Can", "you", "split", "it?")))
      actual should be (expected)
    }

    "Split commas" in {
      val actual = normalizeDoc(TokenSplitter("Find commas, or do not."))
      val expected = makeDoc(Seq(Seq("Find", "commas", ",", "or", "do", "not.")))
      actual should be (expected)
    }

  }

  "A Sentence Splitter" should {
    "Split sentence separators into tokens and sentences" in {
      val actual = normalizeDoc(SentenceSplitter("This is text. Can you split it?"))
      val expected = makeDoc(Seq(Seq("This is text", "."), Seq("Can you split it", "?")))
      actual should be (expected)
    }
  }


}
