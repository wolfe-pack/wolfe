package ml.wolfe.nlp

import ml.wolfe.{nlp, WolfeSpec}

/**
 * @author Sebastian Riedel
 *         Ingolf Becker
 */
class SplitterSpecs extends WolfeSpec {

  import nlp._

  "A Token Splitter" should {
    "Split whitespace" in {
      val actual = Document.normalizeDoc(TokenSplitter("This is text. Can you split it?"))
      val expected = Document(IndexedSeq(IndexedSeq("This", "is", "text.", "Can", "you", "split", "it?")))
      actual should be (expected)
    }

    "Split commas" in {
      val actual = Document.normalizeDoc(TokenSplitter("Find commas, or do not."))
      val expected = Document(IndexedSeq(IndexedSeq("Find", "commas", ",", "or", "do", "not.")))
      actual should be (expected)
    }

    "split sentences" in {
      val sentences = SentenceSplitter("This is text. Can you split it, or not?")
      val tokenizedSentence = Document.normalizeDoc(TokenSplitter(sentences))
      val expected = Document(IndexedSeq(IndexedSeq("This", "is", "text", "."), IndexedSeq("Can", "you", "split", "it", ",", "or", "not", "?")))
      tokenizedSentence should be (expected)
    }

  }

  "A Sentence Splitter" should {
    "Split sentence separators into tokens and sentences" in {
      val actual = Document.normalizeDoc(SentenceSplitter("This is text. Can you split it?"))
      val expected = Document(IndexedSeq(IndexedSeq("This is text", "."), IndexedSeq("Can you split it", "?")))
      actual should be (expected)
    }

    "Split a single sentence of tokens into sentences" in {
      val tokens = TokenSplitter("This is text. Can you split it, or not?")
      val actual = Document.normalizeDoc(SentenceSplitter(tokens))
      val expected = Document(IndexedSeq(IndexedSeq("This", "is", "text", "."), IndexedSeq("Can", "you", "split", "it", ",", "or", "not", "?")))
      actual should be (expected)
    }
  }


}
