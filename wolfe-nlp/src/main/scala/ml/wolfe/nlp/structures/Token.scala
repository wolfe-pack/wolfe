package ml.wolfe.nlp.structures

import ml.wolfe.nlp.generics.GenericTokenCompanion

import scala.language.{implicitConversions,reflectiveCalls}

import scala.collection.immutable.IndexedSeq
/**
 * @author Ingolf Becker
 * @date 31/03/2015
 */
case class Token(word: String, offsets: CharOffsets) extends TokenLike[Token] {
  protected def companion: GenericTokenCompanion[Token] = Token
}

trait TokenLike[T <: TokenLike[T]] {
  this: T =>
  val word: String
  def toPrettyString = word
  val offsets: CharOffsets

  def updatedCopy(newWord: String, newOffsets : CharOffsets) : T = companion.overwriteWord(this, newWord, newOffsets)
  protected def companion : GenericTokenCompanion[T]
}

object Token extends GenericTokenCompanion[Token] {

  def overwriteWord(token: Token, newWord: String, newOffsets: CharOffsets): Token = token.copy(word = newWord, offsets = newOffsets)

  implicit def TokenWithStringToString(x: TokenLike) : String = x.word

  implicit def fromStrings(tokens : Seq[String]) : IndexedSeq[Token] = {
    val lengths = tokens.scanLeft(0)((acc, t) => acc + t.length + 1)
    val properTokens = (tokens, lengths, lengths.drop(1)).zipped.map((word, start, end) => Token(word, CharOffsets(start, end - 1)))
    properTokens.toIndexedSeq
  }
}






case class CharOffsets(start: Int, end: Int)