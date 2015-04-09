package ml.wolfe.nlp.structures

import ml.wolfe.nlp.{CorefAnnotation, DiscourseAnnotation, IRAnnotation}
import ml.wolfe.nlp.generics.GenericDocumentCompanion

import scala.collection.generic.CanBuildFrom
import scala.collection.immutable.IndexedSeq
import scala.collection.{IndexedSeqLike, mutable}

import scala.language.{implicitConversions, higherKinds}

/**
 * @author Ingolf Becker
 * @date 01/04/2015
 */


case class RichDocument[S <: SentenceLike[_ <: TokenLike]](source: String,
                                                          sentences: IndexedSeq[S],
                                                          filename: Option[String] = None,
                                                          id: Option[String] = None,
                                                          ir: IRAnnotation = IRAnnotation.empty,
                                                          coref: CorefAnnotation = CorefAnnotation.empty,
                                                          discourse: DiscourseAnnotation = DiscourseAnnotation.empty)
                                                          extends DocumentLike[S]
                                                          with DocumentWithFilenameLike
                                                          with DocumentWithIdLike
                                                          with DocumentWithIRAnnotationLike
                                                          with DocumentWithCorefAnnotationLike
                                                          with DocumentWithDiscourseAnnotation
                                                          with IndexedSeq[S]
                                                          with IndexedSeqLike[S, RichDocument[S]] {
  override def newBuilder: mutable.Builder[S, RichDocument[S]] = RichDocument.newBuilder
  def toTaggedText = toPrettyString
  def tokens = sentences flatMap (_.tokens)
}

object RichDocument extends GenericDocumentCompanion[RichDocument, SentenceLike, TokenLike] {
  implicit def canBuildFrom[OldS <: SentenceLike[_ <: TokenLike], NewS <: SentenceLike[_ <: TokenLike]]:
  CanBuildFrom[RichDocument[OldS], NewS, RichDocument[NewS]] = newCanBuildFrom
  def fromDocument[NewS <: SentenceLike[_ <: TokenLike]](old: RichDocument[_ <: SentenceLike[_ <: TokenLike]],
                                                         sentences: IndexedSeq[NewS]): RichDocument[NewS] =
    new RichDocument[NewS](old.source,
      sentences,
      old.filename,
      old.id,
      old.ir,
      old.coref,
      old.discourse)

  implicit def fromBaseDocument[OldD <: DocumentLike[_ <: SentenceLike[_ <: TokenLike]]](base: OldD) : RichDocument[RichSentence[RichToken]] = {
    new RichDocument[RichSentence[RichToken]](base.source,{for (s <- base.sentences) yield implicitly[RichSentence[RichToken]](s)})
  }

  override def empty(): RichDocument[SentenceLike[TokenLike]] =
    new RichDocument[SentenceLike[TokenLike]]("",
      IndexedSeq.empty,
      None,
      None,
      IRAnnotation.empty,
      CorefAnnotation.empty,
      DiscourseAnnotation.empty)

  def apply(source: String) : RichDocument[RichSentence[RichToken]] = RichDocument(source, IndexedSeq(RichSentence(IndexedSeq(RichToken(source, CharOffsets(0,source.length))))))



  // Free of var's, but IntelliJ complains :P
  /*
  def apply(sentences:Seq[Seq[String]]) = {
    sentences.foldLeft((0, RichDocument.newBuilder[RichSentence[RichTokenLike]]))({(b, s) =>
    { val x = s.foldLeft((b._1, RichSentence.newBuilder[RichTokenLike]))({(c, t) =>
      (c._1 + t.length + 1, c._2 += RichToken(t,CharOffsets(c._1,c._1 + t.length)))})
      (b._1 + x._1, b._2 += x._2.result())} } )._2.result()
  }
  */

  override implicit def canBuildFromBasic[OldD <: DocumentLike[S], S <: SentenceLike[_ <: TokenLike]]: CanBuildFrom[OldD, S, RichDocument[S]] = newCanBuildFromBasic
  override def fromBasicDocument[OldD <: DocumentLike[S], S <: SentenceLike[_ <: TokenLike]](basic: OldD): RichDocument[S] = new RichDocument[S](basic.source, basic.sentences,
    None,
    None,
    IRAnnotation.empty,
    CorefAnnotation.empty,
    DiscourseAnnotation.empty)

  def apply(sentences:Seq[Seq[String]]) : RichDocument[RichSentence[RichToken]] = {
    val source = sentences.map(_.mkString(" ")).mkString(" ")
    var start = 0
    val resultSentences = for (s <- sentences) yield {
      val tokens = for (t <- s) yield {
        val tmp = RichToken(t, CharOffsets(start, start + t.length))
        start += t.length + 1
        tmp

      }
      RichSentence(tokens.toIndexedSeq)
    }
    RichDocument(source, resultSentences.toIndexedSeq)
  }

  implicit def toDoc(source:String): RichDocument[RichSentence[RichToken]] = RichDocument(source)

  /**
   * Creates a new Document based on the old document, where every token is surrounded by white spaces.
   * @param doc old Document
   * @return A normalised copy of the old Document
   */
  def normalizeDoc(doc : DocumentLike[_ <: SentenceLike[_ <: TokenLike]]) = RichDocument(doc.map(_.map({x : TokenLike => x.word})))
    //doc.map(_.map( { x => x.word }))
}
