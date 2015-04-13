package ml.wolfe.nlp.structures

import ml.wolfe.nlp.generics.GenericSentenceCompanion

import scala.collection.generic.CanBuildFrom
import scala.collection.immutable.IndexedSeq
import scala.collection.{IndexedSeqLike, mutable}

import scala.language.implicitConversions
/**
 * @author Ingolf Becker
 * @date 31/03/2015
 */

trait SentenceBasis

trait SentenceLike[S <: SentenceLike[S, T], T <: TokenLike] extends SentenceBasis with IndexedSeq[T] with IndexedSeqLike[T,SentenceLike[S, T]] {
  this : S =>
  val tokens: IndexedSeq[T]
  def length: Int = tokens.length
  def apply(idx: Int): T = tokens(idx)
  override protected def newBuilder: mutable.Builder[T, SentenceLike[S, T]] = SentenceLike.newBuilder
  def toText = tokens map (_.word) mkString " "
  def offsets = CharOffsets(tokens.head.offsets.start, tokens.last.offsets.end)
  def toPrettyString = tokens.map(_.toPrettyString).mkString(" ")

  protected def companion: GenericSentenceCompanion[S, T]
  def updatedCopy(newTokens: IndexedSeq[T]) : S = companion.overwriteTokens(this, newTokens)
}

object SentenceLike extends GenericSentenceCompanion[SentenceLike, TokenLike]{
  implicit def canBuildFrom[OldT <: TokenLike, NewT <: TokenLike]: CanBuildFrom[SentenceLike[OldT], NewT, SentenceLike[NewT]] = newCanBuildFrom
  override def empty(): SentenceLike[TokenLike] = new Sentence[TokenLike](IndexedSeq.empty)
  def fromSentence[NewT <: TokenLike](old: SentenceLike[_ <: TokenLike], tokens: IndexedSeq[NewT]): SentenceLike[NewT] = new Sentence[NewT](tokens)
  override implicit def canBuildFromBasic[OldS <: SentenceLike[T], T <: TokenLike]: CanBuildFrom[OldS, T, SentenceLike[T]] = newCanBuildFromBasic
  override def fromBasicSentence[OldS <: SentenceLike[T], T <: TokenLike](basicS: OldS): SentenceLike[T] = new Sentence[T](basicS.tokens)

  implicit def sentencesFromTokenSeq[T <: TokenLike](sentseq : Seq[IndexedSeq[T]]) : IndexedSeq[Sentence[T]] = {
    sentseq.map(Sentence(_)).toIndexedSeq
  }
}

case class Sentence[T <: TokenLike](tokens: IndexedSeq[T]) extends SentenceLike[T]
