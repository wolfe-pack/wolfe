package ml.wolfe.nlp

import edu.arizona.sista.processors.{Sentence => SISTASent}

/**
 * Conversion code for the SISTA processor package.
 * @author Sebastian Riedel
 */
object SISTAConverter {

  def toWolfeSentence(sentence: SISTASent): Sentence = {
    val tokens = for (i <- 0 until sentence.size) yield toWolfeToken(i, sentence)
    Sentence(tokens)
  }

  def toWolfeToken(index: Int, sent: SISTASent): Token = {
    def asOption[T](array:Option[Array[T]]):Option[T] = array.map(_.view.apply(index))
    def asNull[T <: AnyRef](array:Option[Array[T]]):T = asOption(array).getOrElse(null.asInstanceOf[T])
    Token(
      word = sent.words(index),
      offsets = CharOffsets(sent.startOffsets(index),sent.endOffsets(index)),
      posTag = asNull(sent.tags),
      attributes = Attributes.empty addOpt (Lemma,asOption(sent.lemmas))
    )
  }

}
