package ml.wolfe

import java.io.File

import ml.wolfe.util.Util

import scala.io.{Source, Codec}
import scala.language.implicitConversions

/**
 * @author Sebastian Riedel
 */
package object nlp {

  implicit def toDoc(source:String) = Document(source, Seq(Sentence(Seq(Token(source,CharOffsets(0,source.length))))))

  def normalizeDoc(doc:Document) = {
    makeDoc(doc.sentences.map(_.tokens.map(_.word)))
  }

  def makeDoc(sentences:Seq[Seq[String]]) = {
    val source = sentences.map(_.mkString(" ")).mkString(" ")
    var start = 0
    val resultSentences = for (s <- sentences) yield {
      val tokens = for (t <- s) yield {
        val tmp = Token(t, CharOffsets(start, start + t.length))
        start += t.size + 1
        tmp
      }
      Sentence(tokens)
    }
    Document(source, resultSentences)
  }

  def loadTxt(file: File, codec: Codec = Codec("ISO8859-1")) = {
    val source = Source.fromFile(file)(codec)
    val content = source.getLines().mkString("\n")
    val doc = toDoc(content)
    source.close()
    doc
  }

  def loadTxts(dir: File, codec: Codec):Seq[Document] = loadTxts(Util.files(dir),codec)

  def loadTxts(files: Seq[File], codec: Codec = Codec("ISO8859-1")):Seq[Document] = {
    files map (loadTxt(_, codec))
  }

}
