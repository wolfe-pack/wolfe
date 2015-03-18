package ml.wolfe

import java.io.File

import ml.wolfe.util.Util

import scala.io.{Source, Codec}

import scala.language.implicitConversions

/**
 * @author Sebastian Riedel
 */
package object nlp {




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
