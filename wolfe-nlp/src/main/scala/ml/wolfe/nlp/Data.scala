package ml.wolfe.nlp


case class CharOffsets(start: Int, end: Int)

/**
 * A natural language token.
 * @param word word at token.
 * @param offsets character offsets
 * @param posTag part-of-speech tag at token.
 * @param attributes collection of generic attributes.
 */
case class Token(word: String, offsets: CharOffsets, posTag: String = null, attributes: Attributes = Attributes.empty) {
  def toTaggedText = word + "/" + posTag
}

/**
 * A sentence consisting of tokens.
 * @param tokens the tokens of the sentence.
 * @param attributes collection of generic attributes.
 */
case class Sentence(tokens: Seq[Token], attributes: Attributes = Attributes.empty) {
  def toText = tokens map (_.word) mkString " "
  def toTaggedText = tokens map (_.toTaggedText) mkString " "
}

/**
 * A document consisting of sentences.
 * @param source the source text.
 * @param sentences list of sentences.
 * @param attributes collection of generic attributes.
 */
case class Document(source: String, sentences: Seq[Sentence], attributes: Attributes = Attributes.empty) {
  def toText = sentences map (_.toText) mkString "\n"
  def toTaggedText = sentences map (_.toTaggedText) mkString "\n"
  def tokens = sentences flatMap (_.tokens)
}


object Data {
  def main(args: Array[String]) {
    val source = "My name is Wolfe."

    val result = SISTAProcessors.mkDocument(source)
    val result2 = SISTAProcessors.annotate(source)

    println(result2.toTaggedText)
    println(result2)


  }
}