package ml.wolfe.nlp.Data.structures

trait TokenWithPOSLike extends TokenLike {
  val posTag: POSTag
  override def toPrettyString: String = word + "/" + posTag.tag
}





