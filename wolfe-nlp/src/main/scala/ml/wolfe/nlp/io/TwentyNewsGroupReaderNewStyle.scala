package ml.wolfe.nlp.io

import java.io.BufferedInputStream

import ml.wolfe.nlp.generics.{GenericDocumentCompanion, GenericSentenceCompanion}
import ml.wolfe.nlp.splitters.TokenSplitter
import ml.wolfe.nlp.{IRAnnotation, IEAnnotation}
import ml.wolfe.nlp.structures._
import ml.wolfe.util.LoggerUtil
import ml.wolfe.util.Util
import ml.wolfe.util.{Util, LoggerUtil}
import org.apache.commons.compress.archivers.tar.TarArchiveInputStream
import org.apache.commons.compress.compressors.gzip.GzipCompressorInputStream

import scala.collection.generic.CanBuildFrom
import scala.collection.{mutable, IndexedSeqLike}
import scala.collection.immutable.IndexedSeq
import scala.collection.mutable.ListBuffer

import scala.language.{higherKinds, implicitConversions}


/**
 * Created by Ingolf on 08/04/2015.
 */

trait TNDocumentLike[S <: SentenceLike[_ <: TokenLike]] extends DocumentLike[S] with DocumentWithIRAnnotationLike  with IndexedSeq[S] with IndexedSeqLike[S,TNDocumentLike[S]] {
  override protected def newBuilder: mutable.Builder[S, TNDocumentLike[S]] = TNDocument.newBuilder
}

object TNDocument extends GenericDocumentCompanion[TNDocumentLike,SentenceLike,TokenLike] {
  override implicit def canBuildFrom[OldS <: SentenceLike[_ <: TokenLike], NewS <: SentenceLike[_ <: TokenLike]]: CanBuildFrom[TNDocumentLike[OldS], NewS, TNDocumentLike[NewS]] = newCanBuildFrom
  override def fromDocument[NewS <: SentenceLike[_ <: TokenLike]](old: TNDocumentLike[_ <: SentenceLike[_ <: TokenLike]], sentences: IndexedSeq[NewS]): TNDocumentLike[NewS] = TNDocument(old.source, sentences, old.ir)
  override implicit def canBuildFromBasic[OldD <: DocumentLike[S], S <: SentenceLike[_ <: TokenLike]]: CanBuildFrom[OldD, S, TNDocumentLike[S]] = newCanBuildFromBasic
  override def fromBasicDocument[OldD <: DocumentLike[S], S <: SentenceLike[_ <: TokenLike]](basic: OldD): TNDocumentLike[S] = new TNDocument[S](basic.source,basic.sentences,IRAnnotation.empty)
}

case class TNDocument[S <: SentenceLike[_ <: TokenLike]](source: String, sentences: IndexedSeq[S], ir: IRAnnotation) extends TNDocumentLike[S]


object TwentyNewsGroupReaderNewStyle {
  def loadFromTarGz(path: String = "ml/wolfe/datasets/20news/20news-bydate.tar.gz") = {
    //http://java-tweets.blogspot.co.uk/2012/07/untar-targz-file-with-apache-commons.html
    LoggerUtil.info("Loading 20 newsgroups ...")
    val stream = Util.getStreamFromClassPathOrFile(path)
    val in = new BufferedInputStream(stream)
    val tarIn = new TarArchiveInputStream(new GzipCompressorInputStream(in))

    var entry = tarIn.getNextEntry
    val trainDocs = new ListBuffer[TNDocument[_ <: SentenceLike[_ <: TokenLike]]]
    val testDocs = new ListBuffer[TNDocument[_ <: SentenceLike[_ <: TokenLike]]]
    while (entry != null) {
      //println(entry.getName + " " + entry.isDirectory)
      if (!entry.isDirectory) {
        val Array(root, label, id) = entry.getName.split("/")
        val content = new Array[Byte](entry.getSize.toInt)
        tarIn.read(content, 0, entry.getSize.toInt)
        val text = new String(content)
        val raw : TNDocument[Sentence[Token]] = TNDocument[Sentence[Token]](text, TNDocument.sentencesFromSeqOfStrings(Seq(Seq(text)))(TokenLike.fromStrings,SentenceLike.sentencesFromTokenSeq), IRAnnotation(docLabel = Some(label)))
        println(raw)
        val doc = TokenSplitter(raw)

        println(doc)
        sys.exit()
        //if (root.endsWith("train")) trainDocs += doc else testDocs += doc
        //
      }
      entry = tarIn.getNextEntry
    }
    in.close()
    (trainDocs, testDocs)
  }

  def main(args: Array[String]) {
    val (train,test) = loadFromTarGz()
    //println(train(0).ir.docLabel)
    //println(train(0).tokens.size)

  }
}
