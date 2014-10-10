package ml.wolfe.apps

import java.io.{FileWriter, BufferedWriter, PrintWriter, File}

import ml.wolfe.Wolfe
import ml.wolfe.nlp.io.IO

import scala.io.Source

/**
 * @author Sebastian Riedel
 */
object KernelWordEmbeddings {

  case class Judgement(word1: String, word2: String, score: Double)


  def pearson(x: Seq[Double], y: Seq[Double]) = {
    val n = x.size
    val d = for ((xi, yi) <- x zip y) yield xi - yi
    val rho = 1.0 - (6.0 * d.view.map(x => x * x).sum / (n * (n * n - 1.0)))
    rho
  }

  def ranked(x: Seq[Double]) = {
    val d = x.distinct
    val ranks = d.sorted.zipWithIndex.toMap
    x.map(ranks(_).toDouble)
  }

  def spearman(x: Seq[Double], y: Seq[Double]) = {
    val rankedX = ranked(x)
    val rankedY = ranked(y)
    pearson(rankedX, rankedY)
  }


  case class Context(words: Array[String]) {
    lazy val feats = Wolfe.toVector(words.zipWithIndex.groupBy(_._1).mapValues(_.length.toDouble))
  }

  type Kernel = (Context, Context) => Double


  def matchKernel(c1: Context, c2: Context) = {
    var sum = 0.0
    for (i <- 0 until c1.words.length) if (c1.words(i) == c2.words(i)) sum += 1
    sum
  }

  def bowKernel(c1: Context, c2: Context) = {
    //each context can be represented as bag of word vector
    //calculate dot product between both
    c1.feats dot c2.feats
  }

  //http://www.jmlr.org/papers/volume13/gretton12a/gretton12a.pdf
  def maxMeanDiscrepancy(kernel: Kernel)(sample1: Seq[Context], sample2: Seq[Context]) = {
    val m = sample1.size
    val n = sample2.size
    var uStats1 = 0.0
    var uStats2 = 0.0
    var sampleAverage = 0.0
    for (i <- sample1.indices; j <- sample1.indices; if i != j) uStats1 += kernel(sample1(i), sample1(j))
    for (i <- sample2.indices; j <- sample2.indices; if i != j) uStats2 += kernel(sample2(i), sample2(j))
    for (i <- sample1.indices; j <- sample2.indices; if i != j) sampleAverage += kernel(sample1(i), sample2(j))
    val result = 1.0 / (m * (m - 1)) * uStats1 + 1.0 / (n * (n - 1)) * uStats2 - 2.0 / (m * n) * sampleAverage
    result
  }

  def maxMeanDiscrepancyLinear(kernel: Kernel)(sample1: Seq[Context], sample2: Seq[Context]) = {
    0.0
  }



  def main(args: Array[String]) {
    val judgements = IO.loadTSV(new File("/Users/sriedel/corpora/wordsim353/combined.tab"), skip = 1) { a =>
      Judgement(a(0), a(1), a(2).toDouble)
    }
    val words = judgements.flatMap(j => j.word1 :: j.word2 :: Nil).distinct.sorted
    val wordSet = words.toSet

    println(spearman(Seq(1.0, 2.0, 3.0), Seq(10.0, 11.0, 12.0)))

    //define kernel

    //calculate spearman correlation

  }

}

object PrepareWikipediaCorpus {
  import KernelWordEmbeddings._

  def main(args: Array[String]) {
    def main(args: Array[String]) {
      val judgements = IO.loadTSV(new File("/Users/sriedel/corpora/wordsim353/combined.tab"), skip = 1) { a =>
        Judgement(a(0), a(1), a(2).toDouble)
      }
      val words = judgements.flatMap(j => j.word1 :: j.word2 :: Nil).distinct.sorted
      println(words)
      IO.saveTSV(words, new File("/Users/sriedel/corpora/wordsim353/words.tab")) { Seq(_) }

      val wordSet = words.toSet

      //go over wikipedia corpus and get contexts for target words
      val out = new PrintWriter(new BufferedWriter(new FileWriter("/Users/sriedel/corpora/WestburyLab.Wikipedia.Corpus/wordsim-contexts.tsv")));
      var contextCount = 0
      val wikipedia = "/Users/sriedel/corpora/WestburyLab.Wikipedia.Corpus/WestburyLab.Wikipedia.Corpus.txt"
      for ((line, i) <- Source.fromFile(wikipedia).getLines().zipWithIndex) {
        if (i % 1000000 == 0) println(i)
        val tokens = Array("<S>", "<S>") ++ line.split(" ") ++ Array("<S>", "<S>")
        for (i <- 2 until tokens.length - 2) {
          if (wordSet(tokens(i))) {
            for (j <- (i - 2) until i + 3) out.print(tokens(j) + "\t")
            contextCount += 1
            out.println()
          }
        }
      }
      println(contextCount)

      //define kernel

      //calculate spearman correlation

    }

  }
}

