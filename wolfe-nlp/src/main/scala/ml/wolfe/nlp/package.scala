package ml.wolfe

import java.io.File

import ml.wolfe.nlp.structures.POSTag
import ml.wolfe.util.Util

import scala.collection.immutable.IndexedSeq
import scala.collection.mutable
import scala.io.{Source, Codec}

import scala.language.implicitConversions

/**
 * @author Sebastian Riedel
 */
package object nlp {


  import ml.wolfe.nlp.structures.{RichDocument, RichSentence, RichToken, CharOffsets => CharOffsetsHere}


  type Document = RichDocument[Sentence]
  val Document = RichDocument

  type Sentence = RichSentence[Token]
  val Sentence = RichSentence

  type Token = RichToken
  val Token = RichToken

  type CharOffsets = CharOffsetsHere
  val CharOffsets = CharOffsetsHere
  implicit def stringToPOSTag(x: String) : POSTag = POSTag(x)

  implicit def arrayToIndexedSeqSentence(x: Array[Sentence]) : IndexedSeq[Sentence] = x.toIndexedSeq

  implicit def indexedSeqToIndexedSeqSentence(x: scala.collection.IndexedSeq[Sentence]) : IndexedSeq[Sentence] = x.toIndexedSeq

  implicit def indexedSeqToIndexedSeqToken(x: scala.collection.IndexedSeq[Token]) : IndexedSeq[Token] = x.toIndexedSeq
  implicit def arrayToIndexedSeqToken(x: Array[Token]) : IndexedSeq[Token] = x.toIndexedSeq

  implicit class SentenceWithExporters(sentence: Sentence) {
    def toCoNNLString: String = {
      // ID FORM LEMMA PLEMMA POS PPOS FEAT PFEAT HEAD PHEAD DEPREL PDEPREL FILLPRED PRED APREDs
      val numPreds = sentence.ie.semanticFrames.size
      sentence.tokens.zipWithIndex.map { case(t, i) =>
        if (sentence.syntax.dependencies != null) {
          val head = sentence.syntax.dependencies.headOf(i+1).getOrElse(-1)
          val headLabel = sentence.syntax.dependencies.labelOf(i+1).getOrElse("null")
          val morph = "-|-|-|-"
          val sense = sentence.ie.semanticFrames.find(_.predicate.idx == i+1) match {
            case Some(frame) => frame.predicate.sense
            case _ => "_"
          }
          val hasPred = if (sense != "_") "Y" else "_"
          val roles = sentence.ie.semanticFrames.map(f => if (f.roles.exists(_.idx == i+1)) f.roles.find(_.idx == i+1).get.role else "_")
          Seq(i+1, t.word, t.lemma, t.lemma, t.posTag, t.posTag, morph, morph, head, head,
            headLabel, headLabel, hasPred, sense, roles.mkString("\t")).mkString("\t")
        }
        else {
          "%d\t%s\t%s\t%s\t%s\t%s".format(i+1, t.word, t.lemma, t.lemma, t.posTag, t.posTag)
        }
      }.mkString("\n")
    }
    /**
     * Return a representation of the entity mentions as a sequence of BIO labels. This representation
     * is different from CoNLL in that every mention begins with B-X.
     */
    def entityMentionsAsBIOSeq : Seq[String] = {
      val tokenIndex2Label = new mutable.HashMap[Int,String]() withDefaultValue "O"
      for (m <- sentence.ie.entityMentions) {
        tokenIndex2Label(m.start) = "B-" + m.label
        for (i <- m.start + 1 until m.end) tokenIndex2Label(i) = "I-" + m.label
      }
      for (i <- 0 until sentence.tokens.size) yield tokenIndex2Label(i)
    }
  }


  def loadTxt(file: File, codec: Codec = Codec("ISO8859-1")) = {

    val source = Source.fromFile(file)(codec)
    val content = source.getLines().mkString("\n")
    val doc = Document(content)
    source.close()
    doc
  }

  def loadTxts(dir: File, codec: Codec):Seq[Document] = loadTxts(Util.files(dir),codec)

  def loadTxts(files: Seq[File], codec: Codec = Codec("ISO8859-1")):Seq[Document] = {
    files map (loadTxt(_, codec))
  }

}
