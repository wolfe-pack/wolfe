package ml.wolfe.nlp.structures

import scala.language.implicitConversions
/**
 * @author Ingolf Becker
 * @date 31/03/2015
 */
case class Token(word: String, offsets: CharOffsets) extends TokenLike

trait TokenLike {
  val word: String
  def toPrettyString = word
  val offsets: CharOffsets
}

object TokenLike {
  implicit def TokenWithStringToString(x: TokenLike) : String = x.word
}





case class CharOffsets(start: Int, end: Int)