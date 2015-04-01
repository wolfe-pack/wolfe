package ml.wolfe.nlp.Data.structures

import ml.wolfe.nlp.{CorefAnnotation, DiscourseAnnotation, IRAnnotation}
import ml.wolfe.nlp.Data.generics.GenericDocumentCompanion

import scala.collection.generic.CanBuildFrom
import scala.collection.immutable.IndexedSeq
import scala.collection.{IndexedSeqLike, mutable}

import scala.language.{implicitConversions, higherKinds}

/**
 * @author Ingolf Becker
 * @date 01/04/2015
 */

trait RichDocumentLike[S <: SentenceLike[_ <: TokenLike]] extends DocumentLike[S] with DocumentWithFilenameLike
                                                              with DocumentWithIdLike
                                                              with DocumentWithIRAnnotationLike
                                                              with DocumentWithCorefAnnotationLike
                                                              with DocumentWithDiscourseAnnotation
                                                              with IndexedSeq[S] with IndexedSeqLike[S, RichDocumentLike[S]] {
  override def newBuilder: mutable.Builder[S, RichDocumentLike[S]] = RichDocumentLike.newBuilder
  def toTaggedText = toPrettyString
}

object RichDocumentLike extends GenericDocumentCompanion[RichDocumentLike, SentenceLike, TokenLike] {
  implicit def canBuildFrom[OldS <: SentenceLike[_ <: TokenLike], NewS <: SentenceLike[_ <: TokenLike]]:
    CanBuildFrom[RichDocumentLike[OldS], NewS, RichDocumentLike[NewS]] = newCanBuildFrom
  def fromDocument[NewS <: SentenceLike[_ <: TokenLike]](old: RichDocumentLike[_ <: SentenceLike[_ <: TokenLike]],
                                                         sentences: IndexedSeq[NewS]): RichDocumentLike[NewS] =
    new RichDocument[NewS](old.source,
      sentences,
      old.filename,
      old.id,
      old.ir,
      old.coref,
      old.discourse)
  def empty(): RichDocumentLike[SentenceLike[TokenLike]] =
    new RichDocument[SentenceLike[TokenLike]]("",
                                              IndexedSeq.empty,
                                              None,
                                              None,
                                              IRAnnotation.empty,
                                              CorefAnnotation.empty,
                                              DiscourseAnnotation.empty)

  def apply(source: String) = RichDocument(source, IndexedSeq(RichSentence(IndexedSeq(RichToken(source, CharOffsets(0,source.length))))))

  // Free of var's, but IntelliJ complains :P
  def apply(sentences:Seq[Seq[String]]) = {
    sentences.foldLeft((0, RichDocumentLike.newBuilder[RichSentenceLike[RichTokenLike]]))({(b, s) =>
    { val x = s.foldLeft((b._1, RichSentenceLike.newBuilder[RichTokenLike]))({(c, t) =>
      (c._1 + t.length + 1, c._2 += RichToken(t,CharOffsets(c._1,c._1 + t.length)))})
      (b._1 + x._1, b._2 += x._2.result())} } )._2.result()
  }
  implicit def toDoc(source:String): RichDocument[RichSentence[RichToken]] = RichDocumentLike(source)

  def normalizeDoc(doc: RichDocumentLike[RichSentenceLike[RichToken]]) = {
    RichDocumentLike(doc.sentences.map(_.tokens.map(_.word)))
  }
}


case class RichDocument[S <: SentenceLike[_ <: TokenLike]](source: String,
                                                           sentences: IndexedSeq[S],
                                                           filename: Option[String] = None,
                                                           id: Option[String] = None,
                                                           ir: IRAnnotation = IRAnnotation.empty,
                                                           coref: CorefAnnotation = CorefAnnotation.empty,
                                                           discourse: DiscourseAnnotation = DiscourseAnnotation.empty)
  extends RichDocumentLike[S]







