package ml.wolfe.nlp.structures

import ml.wolfe.nlp.generics.GenericDocumentCompanion

import scala.collection.generic.CanBuildFrom
import scala.collection.immutable.IndexedSeq
import scala.collection.{IndexedSeqLike, mutable}

/**
 * @author Ingolf Becker
 * @date 01/04/2015
 */

trait DocumentBasis

trait DocumentLike[S <: SentenceLike[_ <: TokenLike]] extends DocumentBasis with IndexedSeq[S] with IndexedSeqLike[S, DocumentLike[S]] {
  val sentences: IndexedSeq[S]
  val source: String
  def length: Int = sentences.size
  def apply(idx: Int): S = sentences(idx)
  override protected def newBuilder: mutable.Builder[S, DocumentLike[S]] = DocumentLike.newBuilder
  def toText = sentences map (_.toText) mkString "\n"
  def toPrettyString = sentences.map(_.toPrettyString) mkString "\n"
}

object DocumentLike extends GenericDocumentCompanion[DocumentLike, SentenceLike, TokenLike] {
  implicit def canBuildFrom[OldS <: SentenceLike[_ <: TokenLike], NewS <: SentenceLike[_ <: TokenLike]]: CanBuildFrom[DocumentLike[OldS], NewS, DocumentLike[NewS]] = newCanBuildFrom
  def fromDocument[NewS <: SentenceLike[_ <: TokenLike]](old: DocumentLike[_ <: SentenceLike[_ <: TokenLike]], sentences: IndexedSeq[NewS]): DocumentLike[NewS] = new Document[NewS](old.source, sentences)
  def empty(): DocumentLike[SentenceLike[TokenLike]] = new Document[SentenceLike[TokenLike]]("",IndexedSeq.empty)

}


case class Document[S <: SentenceLike[_ <: TokenLike]](source: String, sentences: IndexedSeq[S]) extends DocumentLike[S]
