package ml.wolfe.nlp.Data.generics

import scala.collection.generic.CanBuildFrom
import scala.collection.immutable.IndexedSeq
import scala.collection.mutable
import scala.language.{higherKinds, implicitConversions}

/**
 * @author Ingolf Becker
 * @date 31/03/2015
 */
trait GenericDocumentCompanion[Document[_ <: Sentence[_ <: Token]], Sentence[_ <: Token], Token] {

  def newBuilder[S <: Sentence[_ <: Token]] : mutable.Builder[S, Document[S]] = IndexedSeq.newBuilder[S] mapResult(fromDocument(empty(),_))

  implicit def canBuildFrom[OldS <: Sentence[_ <: Token], NewS <: Sentence[_ <: Token]]: CanBuildFrom[Document[OldS], NewS, Document[NewS]]

  def newCanBuildFrom[OldS <: Sentence[_ <: Token], NewS <: Sentence[_ <: Token]] :  CanBuildFrom[Document[OldS], NewS, Document[NewS]] = new CanBuildFrom[Document[OldS], NewS, Document[NewS]] {
    def apply(from: Document[OldS]): mutable.Builder[NewS, Document[NewS]] = IndexedSeq.newBuilder[NewS] mapResult(fromDocument(from, _))
    def apply(): mutable.Builder[NewS, Document[NewS]] = newBuilder
  }

  def fromDocument[NewS <: Sentence[_ <: Token]](old: Document[_ <: Sentence[_ <: Token]], sentences: IndexedSeq[NewS]) : Document[NewS]

  def empty() : Document[Sentence[Token]]
}
