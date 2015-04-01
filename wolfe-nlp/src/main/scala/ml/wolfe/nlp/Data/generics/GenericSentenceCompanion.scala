package ml.wolfe.nlp.Data.generics

import scala.collection.generic.CanBuildFrom
import scala.collection.immutable.IndexedSeq
import scala.collection.mutable
import scala.language.{higherKinds, implicitConversions}

/**
 * @author Ingolf Becker
 * @date 31/03/2015
 */
trait GenericSentenceCompanion[ThisSentence[_ <: TokenBound], TokenBound] {
  def newBuilder[T <: TokenBound] : mutable.Builder[T, ThisSentence[T]] = IndexedSeq.newBuilder[T] mapResult(fromSentence(empty(),_))

  implicit def canBuildFrom[OldT <: TokenBound, NewT <: TokenBound]: CanBuildFrom[ThisSentence[OldT], NewT, ThisSentence[NewT]]

  def newCanBuildFrom[OldT <: TokenBound, NewT <: TokenBound] :  CanBuildFrom[ThisSentence[OldT], NewT, ThisSentence[NewT]] = new CanBuildFrom[ThisSentence[OldT], NewT, ThisSentence[NewT]] {
    def apply(from: ThisSentence[OldT]): mutable.Builder[NewT, ThisSentence[NewT]] = IndexedSeq.newBuilder[NewT] mapResult(fromSentence(from, _))
    def apply(): mutable.Builder[NewT, ThisSentence[NewT]] = newBuilder
  }

  def fromSentence[NewT <: TokenBound](old: ThisSentence[_ <: TokenBound], tokens: IndexedSeq[NewT]) : ThisSentence[NewT]

  def empty() : ThisSentence[TokenBound]
}
