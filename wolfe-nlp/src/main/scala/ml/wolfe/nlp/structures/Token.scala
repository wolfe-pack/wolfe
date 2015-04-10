package ml.wolfe.nlp.structures

import scala.language.{implicitConversions,reflectiveCalls}

import scala.collection.immutable.IndexedSeq
/**
 * @author Ingolf Becker
 * @date 31/03/2015
 */
case class Token(word: String, offsets: CharOffsets) extends TokenLike[Token]

trait TokenLike[T <: TokenLike[T]] {
  this: {def copy(word: String, offsets: CharOffsets): T} =>
  val word: String
  def toPrettyString = word
  val offsets: CharOffsets
  def myCopy(word: String, offsets: CharOffsets): T = copy(word, offsets)
}

object TokenLike {
  implicit def TokenWithStringToString(x: TokenLike) : String = x.word

  implicit def fromStrings(tokens : Seq[String]) : IndexedSeq[Token] = {
    val lengths = tokens.scanLeft(0)((acc, t) => acc + t.length + 1)
    val properTokens = (tokens, lengths, lengths.drop(1)).zipped.map((word, start, end) => Token(word, CharOffsets(start, end - 1)))
    properTokens.toIndexedSeq
  }
}





case class CharOffsets(start: Int, end: Int)