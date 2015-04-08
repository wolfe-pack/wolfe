package ml.wolfe.nlp

import splitters.{TokenSplitter => TS, SentenceSplitter => SS}

/**
 * @author Sebastian Riedel
 */
object TokenSplitter extends (Document => Document) {

  def apply(doc: Document): Document = {
    TS(doc)
  }

  def main(args: Array[String]) {

    val tokenized = TokenSplitter("This is Wolfe. What are you? Blah blub")
    println(tokenized.toText)
    println(tokenized)
    val split = SentenceSplitter(tokenized)
    println(split)
    println(split.toText)
    val split2 = SentenceSplitter("This is Wolfe. What are you? Blah blub")

    println(split2)
    println(split2.toText)
    val tokenized2 = TokenSplitter(split2)
    println(tokenized2 )
    println(tokenized2.toText)
    val normTok = Document.normalizeDoc(tokenized)
    println(normTok)
    println(normTok == tokenized)
    println(split == tokenized2)


    val actual = TokenSplitter("Find commas, or do not.")
    val expected = Document(IndexedSeq(IndexedSeq("Find", "commas", ",", "or", "do", "not.")))

    for ( (s1, s2) <- actual.sentences.zip(expected.sentences)) {
        if (s1 != s2 ) println(s1.toString + " does not equal " + s2.toString)
        for ( (t1, t2) <- s1.tokens.zip(s2.tokens)) {
          if (t1 != t2) println(t1.toString + " does not equal " +t2.toString)
        }
    }

  }
}

object SentenceSplitter extends (Document => Document) {

  def apply(doc: Document) : Document = {
    SS(doc)
  }

}
