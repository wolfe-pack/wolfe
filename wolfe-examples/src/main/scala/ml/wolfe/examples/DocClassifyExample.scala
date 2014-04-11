package ml.wolfe.examples

import ml.wolfe.Wolfe
import ml.wolfe.util.{Evaluator, LoggerUtil, Util, NLP}
import ml.wolfe.macros.{Library, OptimizedOperators}
import java.io.BufferedInputStream
import org.apache.commons.compress.compressors.gzip._
import org.apache.commons.compress.archivers.tar.TarArchiveInputStream
import scala.collection.mutable.ListBuffer
import cc.factorie.app.strings.alphaSegmenter
import cc.factorie.optimize.{Perceptron, OnlineTrainer}
import scala.util.Random

/**
 * @author Sebastian Riedel
 */
object DocClassifyExample {

  import Wolfe._
  import NLP._
  import OptimizedOperators._
  import Library._

  class Model(labels: Iterable[DocLabel]) {
    def Docs = all(Doc) { strings x infty[Seq[Token]] x labels }

    def observed(d: Doc) = d.copy(label = hidden)

    @Atomic
    def features(d: Doc) = sum { over(0 until d.tokens.size) of (i => oneHot(d.label -> d.tokens(i).word)) }

    def model(w: Vector)(d: Doc) = w dot features(d)

    def predictor(w: Vector)(d: Doc) = argmax { over(Docs) of model(w) st evidence(observed)(d) }

    def batchPredictor(w:Vector)(data:Iterable[Doc]) = map { over(data) using predictor(w)}

    @OptimizeByLearning(new OnlineTrainer(_, new Perceptron, 1, 50))
    def loss(data: Iterable[Doc])(w: Vector) = sum { over(data) of (s => model(w)(predictor(w)(s)) - model(w)(s)) }

    def learn(data: Iterable[Doc]) = argmin { over[Vector] of loss(data) }

  }
  def main(args: Array[String]) {
    val random = new Random(0l)
    val (train, test) = Load20NewsGroups.loadFromTarGz()
    val sub = random.shuffle(train)
    val labels = train.map(_.label).distinct
    println(labels.size)
    println(labels.mkString(", "))
    val model = new Model(labels)
    val w = model.learn(sub)
    println(w.take(10))
    println("Prediction ... ")
    val trainPredicted = model.batchPredictor(w)(sub)
    val trainEval = Evaluator.evaluate(sub,trainPredicted)(_.label)
    println(trainEval)

    //load 20 newsgroups data
  }

}

object Load20NewsGroups {

  def main(args: Array[String]) {
    val (train, test) = loadFromTarGz()
    println(train.size)
    println(test.size)
    println(train.head.tokens.size)
  }

  def loadFromTarGz(path: String = "ml/wolfe/datasets/20news/20news-bydate.tar.gz") = {
    import NLP._
    //http://java-tweets.blogspot.co.uk/2012/07/untar-targz-file-with-apache-commons.html
    LoggerUtil.info("Loading 20 newsgroups ...")
    val stream = Util.getStreamFromClassPathOrFile(path)
    val in = new BufferedInputStream(stream)
    val gzIn = new GzipCompressorInputStream(in)
    val tarIn = new TarArchiveInputStream(gzIn)

    var entry = tarIn.getNextEntry
    val trainDocs = new ListBuffer[Doc]
    val testDocs = new ListBuffer[Doc]
    while (entry != null) {
      //println(entry.getName + " " + entry.isDirectory)
      if (!entry.isDirectory) {
        val Array(root, label, id) = entry.getName.split("/")
        val content = new Array[Byte](entry.getSize.toInt)
        tarIn.read(content, 0, entry.getSize.toInt)
        val text = new String(content)
        def toToken(string: String) = Token(string)
        val tokens = alphaSegmenter(text).map(toToken).toIndexedSeq
        val doc = Doc(text, tokens, DocLabel(label))
        if (root.endsWith("train")) trainDocs += doc else testDocs += doc
        //
      }
      entry = tarIn.getNextEntry
    }
    in.close()
    (trainDocs, testDocs)

  }

}