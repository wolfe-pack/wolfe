package ml.wolfe.nlp.Data.structures

import ml.wolfe.nlp.{IEAnnotation, SyntaxAnnotation}
import ml.wolfe.nlp.Data.generics.GenericSentenceCompanion

import scala.collection.generic.CanBuildFrom
import scala.collection.immutable.IndexedSeq
import scala.collection.{IndexedSeqLike, mutable}

/**
 * @author Ingolf Becker
 * @date 01/04/2015
 */


trait RichSentenceLike[T <: TokenLike] extends SentenceLike[T] with SentenceWithSyntaxAnnotationLike
                                               with SentenceWithIEAnnotationLike
                                               with IndexedSeq[T] with IndexedSeqLike[T,SentenceLike[T]] {
  override def newBuilder: mutable.Builder[T, RichSentenceLike[T]] = RichSentenceLike.newBuilder
  def toTaggedText = toPrettyString
}

object RichSentenceLike extends GenericSentenceCompanion[RichSentenceLike,TokenLike] {
  implicit def canBuildFrom[OldT <: TokenLike, NewT <: TokenLike]: CanBuildFrom[RichSentenceLike[OldT], NewT, RichSentenceLike[NewT]] =
    newCanBuildFrom
  def empty(): RichSentenceLike[TokenLike] = new RichSentence[TokenLike](IndexedSeq.empty, SyntaxAnnotation.empty, IEAnnotation.empty)
  def fromSentence[NewT <: TokenLike](old: RichSentenceLike[_ <: TokenLike], tokens: IndexedSeq[NewT]): RichSentenceLike[NewT] =
    new RichSentence(tokens, old.syntax, old.ie)

}

case class RichSentence[T <: TokenLike](tokens: IndexedSeq[T],
                                        syntax: SyntaxAnnotation = SyntaxAnnotation.empty,
                                        ie: IEAnnotation = IEAnnotation.empty) extends RichSentenceLike[T]
