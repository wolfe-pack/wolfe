package ml.wolfe.nlp.io

import java.io.BufferedInputStream

import ml.wolfe.nlp.Document
import ml.wolfe.util.{Util, LoggerUtil}
import org.apache.commons.compress.archivers.tar.TarArchiveInputStream
import org.apache.commons.compress.compressors.gzip.GzipCompressorInputStream

import scala.collection.mutable.ListBuffer

import ml.wolfe.nlp._

/**
 * @author riedel
 */
object TwentyNewsGroupReader {

  def loadFromTarGz(path: String = "ml/wolfe/datasets/20news/20news-bydate.tar.gz") = {
    //http://java-tweets.blogspot.co.uk/2012/07/untar-targz-file-with-apache-commons.html
    LoggerUtil.info("Loading 20 newsgroups ...")
    val stream = Util.getStreamFromClassPathOrFile(path)
    val in = new BufferedInputStream(stream)
    val gzIn = new GzipCompressorInputStream(in)
    val tarIn = new TarArchiveInputStream(gzIn)

    var entry = tarIn.getNextEntry
    val trainDocs = new ListBuffer[Document]
    val testDocs = new ListBuffer[Document]
    while (entry != null) {
      //println(entry.getName + " " + entry.isDirectory)
      if (!entry.isDirectory) {
        val Array(root, label, id) = entry.getName.split("/")
        val content = new Array[Byte](entry.getSize.toInt)
        tarIn.read(content, 0, entry.getSize.toInt)
        val text = new String(content)
        val raw = toDoc(text)
        val doc = TokenSplitter(raw).copy(ir = IRAnnotation(docLabel = Some(label)))
        if (root.endsWith("train")) trainDocs += doc else testDocs += doc
        //
      }
      entry = tarIn.getNextEntry
    }
    in.close()
    (trainDocs, testDocs)

  }

  def main(args: Array[String]) {
    val (train,test) = loadFromTarGz()
    println(train(0).ir.docLabel)
    println(train(0).tokens.size)
  }

}
