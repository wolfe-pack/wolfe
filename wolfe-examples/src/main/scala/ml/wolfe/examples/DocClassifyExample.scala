package ml.wolfe.examples

import ml.wolfe.Wolfe
import ml.wolfe.util.{Util, NLP}
import ml.wolfe.macros.{Library, OptimizedOperators}
import java.io.BufferedInputStream
import org.apache.commons.compress.compressors.gzip._
import org.apache.commons.compress.archivers.tar.{TarArchiveEntry, TarArchiveInputStream}
import scala.collection.mutable.ListBuffer

/**
 * @author Sebastian Riedel
 */
object DocClassifyExample {

  import Wolfe._
  import NLP._
  import OptimizedOperators._
  import Library._

  def Docs = all(Doc)(strings x seqs(all(Token)) x Seq.empty[DocLabel])

  def observed(d: Doc) = d.copy(label = hide[DocLabel])

  def features(d: Doc) = sum { over(0 until d.tokens.size) of (i => oneHot(d.label -> d.tokens(i).word)) }

  def model(w: Vector)(d: Doc) = w dot features(d)

  def predictor(w: Vector)(d: Doc) = argmax { over(Docs) of model(w) st evidence(observed)(d) }

  def loss(data: Iterable[Doc])(w: Vector) = sum { over(data) of (s => model(w)(predictor(w)(s)) - model(w)(s)) }

  def learn(data: Iterable[Doc]) = argmin { over[Vector] of loss(data) }

  def main(args: Array[String]) {
    //load 20 newsgroups data
  }

}

object Load20NewsGroups {

  def main(args: Array[String]) {
    val (train,test) = loadFromTarGz()
    println(train.size)
    println(test.size)
  }

  def loadFromTarGz(path:String = "ml/wolfe/datasets/20news/20news-bydate.tar.gz") = {
    import NLP._
    //http://java-tweets.blogspot.co.uk/2012/07/untar-targz-file-with-apache-commons.html
    val stream = Util.getStreamFromClassPathOrFile(path)
    val in = new BufferedInputStream(stream)
    val gzIn = new GzipCompressorInputStream(in)
    val tarIn = new TarArchiveInputStream(gzIn)

    var entry = tarIn.getNextEntry
    val trainDocs = new ListBuffer[Doc]
    val testDocs = new ListBuffer[Doc]
    while (entry != null) {
      println(entry.getName + " " + entry.isDirectory)
      if (!entry.isDirectory) {
        val Array(root, label, id) = entry.getName.split("/")
        val content = new Array[Byte](entry.getSize.toInt)
        tarIn.read(content, 0, entry.getSize.toInt)
        val text = new String(content)
        println(text.take(30))
        val doc = Doc(text, null, DocLabel(label))
        if (root.endsWith("train")) trainDocs += doc else testDocs += doc
        //
      }
      entry = tarIn.getNextEntry
    }
    in.close()
    (trainDocs,testDocs)

  }

}