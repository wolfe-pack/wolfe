package ml.wolfe.nlp.structures

trait TokenWithPOSLike extends TokenLike {
  val posTag: POSTag
  override def toPrettyString: String = word + "/" + posTag.tag
}





