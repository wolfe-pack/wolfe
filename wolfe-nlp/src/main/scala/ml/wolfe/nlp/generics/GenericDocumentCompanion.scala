package ml.wolfe.nlp.generics

import ml.wolfe.nlp.structures._
import ml.wolfe.nlp.structures.{Document => BasicDocument}

import scala.collection.generic.CanBuildFrom
import scala.collection.immutable.IndexedSeq
import scala.collection.mutable
import scala.language.{higherKinds, implicitConversions,postfixOps}

/**
 * @author Ingolf Becker
 * @date 31/03/2015
 */
trait GenericDocumentCompanion[Document[_ <: Sentence[_ <: Token]] <: DocumentLike[_ <: SentenceLike[_ <: TokenLike]], Sentence[_ <: Token] <: SentenceLike[_ <: TokenLike], Token <: TokenLike] {

  def newBuilder[S <: Sentence[_ <: Token]]: mutable.Builder[S, Document[S]] = IndexedSeq.newBuilder[S] mapResult (fromDocument(empty(), _))

  implicit def canBuildFrom[OldS <: Sentence[_ <: Token], NewS <: Sentence[_ <: Token]]: CanBuildFrom[Document[OldS], NewS, Document[NewS]]

  def newCanBuildFrom[OldS <: Sentence[_ <: Token], NewS <: Sentence[_ <: Token]]: CanBuildFrom[Document[OldS], NewS, Document[NewS]] = new CanBuildFrom[Document[OldS], NewS, Document[NewS]] {
    def apply(from: Document[OldS]): mutable.Builder[NewS, Document[NewS]] = IndexedSeq.newBuilder[NewS] mapResult (fromDocument(from, _))
    def apply(): mutable.Builder[NewS, Document[NewS]] = newBuilder
  }

  def fromDocument[NewS <: Sentence[_ <: Token]](old: Document[_ <: Sentence[_ <: Token]], sentences: IndexedSeq[NewS]): Document[NewS]

  def empty(): Document[Sentence[Token]] = fromBasicDocument(DocumentLike.newBuilder[Sentence[Token]].result())

  implicit def canBuildFromBasic[OldD <: DocumentLike[S], S <: Sentence[_ <: Token]]: CanBuildFrom[OldD, S, Document[S]]

  def newCanBuildFromBasic[OldD <: DocumentLike[S], S <: Sentence[_ <: Token]]: CanBuildFrom[OldD, S, Document[S]] = new CanBuildFrom[OldD, S, Document[S]] {
    override def apply(from: OldD): mutable.Builder[S, Document[S]] = IndexedSeq.newBuilder[S] mapResult {
      val d: Document[S] = fromBasicDocument(from)
      fromDocument(d, _)
    }
    override def apply(): mutable.Builder[S, Document[S]] = newBuilder
  }

  def fromBasicDocument[OldD <: DocumentLike[S], S <: Sentence[_ <: Token]](basic: OldD): Document[S]

  /*  implicit def stringsToSentences(sentences: Seq[Seq[String]])(implicit stringToToken: (String => Token),
                                             tokenToSentence: CanBuildFrom[SentenceLike[Token], Token, Sentence[Token]]) : IndexedSeq[Sentence[Token]] = {
    val tokens = sentences.flatten.map(stringToToken)
    val lengths = sentences.scanLeft(0)((acc, t) => acc + t.size + 1)

    val properSentences = (lengths, lengths.drop(1)).zipped.map((start, end) => tokenToSentence(Sentence(tokens.slice(start, end - 1).toIndexedSeq)).result()).toIndexedSeq
    properSentences
  }

  def apply2(sentences:Seq[Seq[String]])(implicit stringToToken: (String => Token),
                                        tokenToSentence: CanBuildFrom[SentenceLike[Token], Token, Sentence[Token]]): Document[Sentence[Token]] = {
    val tokens = sentences.flatten.map(stringToToken)
    val lengths = sentences.scanLeft(0)((acc, t) => acc + t.size + 1)

    val properSentences = (lengths, lengths.drop(1)).zipped.map((start, end) => tokenToSentence(Sentence(tokens.slice(start, end - 1).toIndexedSeq)).result()).toIndexedSeq

    val source = sentences.map(_.mkString(" ")).mkString(" ")
    fromBasicDocument(new BasicDocument[Sentence[Token]](source, properSentences))
  }*/
  //
  //  def apply3(sentences:Seq[Seq[String]])(implicit stringToToken: (String => Token),
  //                                        tokenToSentence: (Seq[Token] => Sentence[Token])): Document[Sentence[Token]] = {
  //    val tokens = sentences.flatten.map(stringToToken)
  //    val lengths = sentences.scanLeft(0)((acc, t) => acc + t.size + 1)
  //
  //    val properSentences = (lengths, lengths.drop(1)).zipped.map((start, end) => tokenToSentence(tokens.slice(start, end - 1))).toIndexedSeq
  //
  //    val source = sentences.map(_.mkString(" ")).mkString(" ")
  //    fromBasicDocument(new BasicDocument[Sentence[Token]](source, properSentences))
  //  }
  //
  implicit def sentencesFromSeqOfStrings[S <: Sentence[T], T <: Token](sentences: Seq[Seq[String]])(implicit cbToken: (Seq[String] => IndexedSeq[T]),
                                                                                                    cbSentence: (Seq[IndexedSeq[T]] => IndexedSeq[S])): IndexedSeq[S] = {
    val tokens = cbToken(sentences.flatten)
    val lengths = sentences.scanLeft(0)((acc, t) => acc + t.size + 1)
    val properSentences = cbSentence({for {(start, end) <- (lengths, lengths.drop(1)) zipped} yield tokens.slice(start, end - 1)}.toSeq)
    properSentences
  }
//
//  implicit def SentencesFromSeqOfStringsOld[S <: Sentence[T], T <: Token](sentences: Seq[Seq[String]])(implicit cbToken: (Seq[String] => IndexedSeq[T]),
//                                                                                                    cbSentence: CanBuildFrom[Seq[IndexedSeq[T]], S, IndexedSeq[S]]): IndexedSeq[S] = {
//    val tokens = cbToken(sentences.flatten)
//    val lengths = sentences.scanLeft(0)((acc, t) => acc + t.size + 1)
//    val properSentences = cbSentence(for {(start, end) <- (lengths, lengths.drop(1)) zipped} yield tokens.slice(start, end - 1)).result()
//    properSentences
//  }
//
//
//  def apply2(sentences: Seq[Seq[String]])(implicit cbToken: (Seq[String] => Seq[Token]),
//                                          cbSentence: CanBuildFrom[Seq[IndexedSeq[Token]], Sentence[Token], IndexedSeq[Sentence[Token]]]): Document[Sentence[Token]] = {
//    val tokens = cbToken(sentences.flatten).toIndexedSeq
//    val lengths = sentences.scanLeft(0)((acc, t) => acc + t.size + 1)
//    cbSentence()
//
//    val properSentences = cbSentence(for {(start, end) <- (lengths, lengths.drop(1)) zipped} yield tokens.slice(start, end - 1)).result()
//    val source = sentences.map(_.mkString(" ")).mkString(" ")
//    fromBasicDocument(new BasicDocument[Sentence[Token]](source, properSentences))
//  }
}
