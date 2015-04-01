package ml.wolfe.nlp.structures

import ml.wolfe.nlp.{IEAnnotation, SyntaxAnnotation}
import ml.wolfe.nlp.generics.GenericSentenceCompanion

import scala.collection.generic.CanBuildFrom
import scala.collection.immutable.IndexedSeq
import scala.collection.{IndexedSeqLike, mutable}

/**
 * @author Ingolf Becker
 * @date 01/04/2015
 */

case class RichSentence[T <: TokenLike](tokens: IndexedSeq[T],
                                        syntax: SyntaxAnnotation = SyntaxAnnotation.empty,
                                        ie: IEAnnotation = IEAnnotation.empty) extends SentenceLike[T]
                                                     with SentenceWithSyntaxAnnotationLike
                                                     with SentenceWithIEAnnotationLike
                                                     with IndexedSeq[T] with IndexedSeqLike[T,SentenceLike[T]] {
  override def newBuilder: mutable.Builder[T, RichSentence[T]] = RichSentence.newBuilder
  def toTaggedText = toPrettyString
}

object RichSentence extends GenericSentenceCompanion[RichSentence, TokenLike] {
  implicit def canBuildFrom[OldT <: TokenLike, NewT <: TokenLike]: CanBuildFrom[RichSentence[OldT], NewT, RichSentence[NewT]] = newCanBuildFrom
  def empty(): RichSentence[TokenLike] = new RichSentence[TokenLike](IndexedSeq.empty)
  def fromSentence[NewT <: TokenLike](old: RichSentence[_ <: TokenLike], tokens: IndexedSeq[NewT]): RichSentence[NewT] = new RichSentence[NewT](tokens, old.syntax, old.ie)
}