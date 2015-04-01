package ml.wolfe.nlp.structures

import ml.wolfe.nlp.generics.GenericSentenceCompanion

import scala.collection.generic.CanBuildFrom
import scala.collection.immutable.IndexedSeq
import scala.collection.{IndexedSeqLike, mutable}

/**
 * @author Ingolf Becker
 * @date 31/03/2015
 */

trait SentenceBasis

trait SentenceLike[T <: TokenLike] extends SentenceBasis with IndexedSeq[T] with IndexedSeqLike[T,SentenceLike[T]] {
  val tokens: IndexedSeq[T]
  def length: Int = tokens.length
  def apply(idx: Int): T = tokens(idx)
  override protected def newBuilder: mutable.Builder[T, SentenceLike[T]] = SentenceLike.newBuilder
  def toText = tokens map (_.word) mkString " "
  def offsets = CharOffsets(tokens.head.offsets.start, tokens.last.offsets.end)
  def toPrettyString = tokens.map(_.toPrettyString).mkString(" ")
}

object SentenceLike extends GenericSentenceCompanion[SentenceLike, TokenLike]{
  implicit def canBuildFrom[OldT <: TokenLike, NewT <: TokenLike]: CanBuildFrom[SentenceLike[OldT], NewT, SentenceLike[NewT]] = newCanBuildFrom
  def empty(): SentenceLike[TokenLike] = new Sentence[TokenLike](IndexedSeq.empty)
  def fromSentence[NewT <: TokenLike](old: SentenceLike[_ <: TokenLike], tokens: IndexedSeq[NewT]): SentenceLike[NewT] = new Sentence[NewT](tokens)

}

case class Sentence[T <: TokenLike](tokens: IndexedSeq[T]) extends SentenceLike[T]
