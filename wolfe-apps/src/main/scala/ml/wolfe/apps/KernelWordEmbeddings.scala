package ml.wolfe.apps

import java.io.{FileWriter, BufferedWriter, PrintWriter, File}

import scalaxy.loops._

import ml.wolfe.Wolfe
import ml.wolfe.nlp.io.IO

import scala.collection.mutable
import scala.io.Source
import scala.util.Random

/**
 * @author Sebastian Riedel
 */
object KernelWordEmbeddings {

  val SentStart = "<s>"
  def contextsFile(window: Int = 2) = s"/Users/sriedel/corpora/WestburyLab.Wikipedia.Corpus/wordsim-contexts-$window.tsv"

  val wordWeights = (s:String) => if (s != SentStart) 1.0 else 0.0 //(Stopwords.all.map(_ -> 0.0).toMap + (SentStart->0.0)) withDefaultValue 1.0 //(s:String) => 1.0 //


  case class Judgement(word1: String, word2: String, score: Double)

  case class Context(words: Array[String], feats: Wolfe.Vector = Wolfe.VectorZero) {
    def render(word:String) =
      (words.view.take(words.size / 2) ++ Seq("|" + word + "|") ++ words.view.takeRight(words.size / 2)).mkString(" ")
  }

  case class WordRepresentation(word: String, contexts: Seq[Context])

  def bowFeats(words: Array[String]) = Wolfe.toVector(words.zipWithIndex.groupBy(_._1).mapValues(_.length.toDouble))

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


  type Kernel = (Context, Context) => Double


  def matchKernel(c1: Context, c2: Context) = {
    var sum = 0.0
    for (i <- (0 until c1.words.length).optimized) if (c1.words(i) == c2.words(i)) sum += 1
    sum
  }

  def pairwiseKernel(c1: Context, c2: Context) = {
    var sum = 0.0
    for (i <- (0 until c1.words.length).optimized; j <- (0 until c2.words.length).optimized)
      if (c1.words(i) == c2.words(j)) sum += wordWeights(c1.words(i))
    sum
  }

  def bowKernel(c1: Context, c2: Context) = {
    //each context can be represented as bag of word vector
    //calculate dot product between both
    c1.feats dot c2.feats
  }

  def ustats(kernel: Kernel)(sample1: Seq[Context]) = {
    val m = sample1.size
    var uStats1 = 0.0
    for (i <- sample1.indices; j <- sample1.indices; if i != j) uStats1 += kernel(sample1(i), sample1(j))
    1.0 / (m * (m - 1)) * uStats1
  }

  def sampleAvg(kernel: Kernel)(sample1: Seq[Context], sample2: Seq[Context]) = {
    val m = sample1.size
    val n = sample2.size
    var sampleAverage = 0.0
    for (i <- sample1.indices; j <- sample2.indices; if i != j) sampleAverage += kernel(sample1(i), sample2(j))
    2.0 / (m * n) * sampleAverage
  }

  //http://www.jmlr.org/papers/volume13/gretton12a/gretton12a.pdf
  def maxMeanDiscrepancy(kernel: Kernel)(sample1: Seq[Context], sample2: Seq[Context]): Double = {
    val m = sample1.size
    val n = sample2.size
    var uStats1 = 0.0
    var uStats2 = 0.0
    var sampleAverage = 0.0
    for (i <- sample1.indices; j <- sample1.indices; if i != j) uStats1 += kernel(sample1(i), sample1(j))
    for (i <- sample2.indices; j <- sample2.indices; if i != j) uStats2 += kernel(sample2(i), sample2(j))
    for (i <- sample1.indices; j <- sample2.indices; if i != j) sampleAverage += kernel(sample1(i), sample2(j))
    val result = ustats(kernel)(sample1) * uStats1 + ustats(kernel)(sample2) - sampleAvg(kernel)(sample1, sample2)
    result
  }

  //http://www.jmlr.org/papers/volume13/gretton12a/gretton12a.pdf


  def maxMeanDiscrepancyLinear(kernel: Kernel)(sample1: Seq[Context], sample2: Seq[Context]) = {
    0.0
  }


  def main(args: Array[String]) {

    val debug = true

    val allJudgements = "/Users/sriedel/corpora/wordsim353/combined.tab"
    val judgementSubset = "/Users/sriedel/corpora/wordsim353/tiger-difficult.tab"
    val judgements = IO.loadTSV(new File(judgementSubset), skip = 0) { a =>
      Judgement(a(0), a(1), a(2).toDouble)
    }
    val words = judgements.flatMap(j => j.word1 :: j.word2 :: Nil).distinct.sorted
    val wordSet = words.toSet

    println(spearman(Seq(2.0, 1.0), Seq(1.0, 2.0)))

    val random = new Random(0)
    val acceptProb = 0.9

    val windowSize = 5


    def loadContexts(maxContexts: Int = Int.MaxValue) = {
      val contexts = new mutable.HashMap[String, List[Context]]() withDefaultValue Nil
      IO.processTSV(new File(contextsFile(windowSize))) { a =>
        if (wordSet(a(0)) && random.nextDouble() > acceptProb) contexts(a(0)) = Context(a.drop(1).map(_.toLowerCase)) :: contexts(a(0))
      }
      val result = contexts.mapValues(random.shuffle(_).take(maxContexts))
      result
    }

    val shortened = loadContexts(1000)
    println(shortened.map(_._2.size).min)
    println(shortened.map(_._2.size).max)

    val featureFunction = bowFeats(_)

    val withFeatures = shortened.par.mapValues(_.map(c => c.copy(feats = featureFunction(c.words)))).seq

    println("Calculated Features")

    val kernel = pairwiseKernel(_, _) //matchKernel(_, _) //bowKernel(_,_)

    val embeddings = withFeatures.par.map { pair =>
      val (word, contexts) = pair
      val asSeq = contexts.toIndexedSeq
      word -> WordRepresentation(word, asSeq)
    }.seq
    println("Done generating embeddings")

    val ustatsForKernel = embeddings.par.map(e => e._1 -> ustats(kernel)(e._2.contexts))

    println("Done calculating ustats")
    //define kernel


    //calculate MMD
    val predictions = for (judgement <- judgements) yield {
      val emb1 = embeddings(judgement.word1)
      val emb2 = embeddings(judgement.word2)
      val avg = sampleAvg(kernel)(emb1.contexts, emb2.contexts)
      val ustats1 = ustatsForKernel(emb1.word)
      val ustats2 = ustatsForKernel(emb2.word)
      val mmd = - avg + ustats1 + ustats2
      println("=========")
      println(s"${ emb1.word } vs ${ emb2.word }, mmd: $mmd score: ${ judgement.score }")
      println(s"ustats1: $ustats1 (${emb1.contexts.size}), ustats2:$ustats2 (${emb2.contexts.size}), sampleAvg:$avg")
      if (debug) {

        println("------")
        println(emb1.contexts.take(10).map(s => s.render(emb1.word)).mkString("\n"))
        println("------")
        println(emb2.contexts.take(10).map(s => s.render(emb2.word)).mkString("\n"))
        println("------")

        val kernelEvals = for (c1 <- emb1.contexts; c2 <- emb2.contexts; k = kernel(c1, c2); if k != 0.0) yield
          (c1, c2, k)

        val allPairCount = emb1.contexts.size * emb2.contexts.size
        val sorted = kernelEvals.sortBy(-_._3)
        val groupByScore = sorted.groupBy(_._3).mapValues(_.length)
        println(s"(0.0,${allPairCount - kernelEvals.size}, ${(allPairCount - kernelEvals.size)/allPairCount.toDouble})")
        println(groupByScore.toSeq.sortBy(_._1).map(p => (p._1,p._2,p._2 /allPairCount.toDouble ))mkString("\n"))

        for ((c1, c2, k) <- sorted.take(100))
          println( s"""$k\n${ c1.render(emb1.word) }\n${ c2.render(emb2.word) }""")

      }

      if (mmd.isNaN) 0.0 else -mmd
    }
    println("Done calculating similarities")


    val target = judgements.map(_.score)
    val coeff = spearman(predictions, target)

    println("Spearman: " + coeff)

    //maybe using http://arxiv.org/abs/1409.2802
    //Fast multipole methods: http://math.nyu.edu/faculty/greengar/shortcourse_fmm.pdf? (No, limited in dimensions)

    //calculate spearman correlation

  }

}

object PrepareWikipediaCorpus {

  import KernelWordEmbeddings._

  def normalize(word: String) = word.replaceAll("[,.()]", "")

  def main(args: Array[String]) {
    val windowSize = args.lift(0).map(_.toInt).getOrElse(2)
    val judgements = IO.loadTSV(new File("/Users/sriedel/corpora/wordsim353/combined.tab"), skip = 1) { a =>
      Judgement(a(0), a(1), a(2).toDouble)
    }
    val words = judgements.flatMap(j => j.word1 :: j.word2 :: Nil).distinct.sorted
    println(words)
    IO.saveTSV(words, new File("/Users/sriedel/corpora/wordsim353/words.tab")) { IndexedSeq(_) }

    val wordSet = words.toSet

    val padding = Array.fill(windowSize)(SentStart)

    //go over wikipedia corpus and get contexts for target words
    val out = new PrintWriter(new BufferedWriter(new FileWriter(contextsFile(windowSize))))
    var contextCount = 0
    val wikipedia = "/Users/sriedel/corpora/WestburyLab.Wikipedia.Corpus/WestburyLab.Wikipedia.Corpus.txt"
    for ((line, i) <- Source.fromFile(wikipedia).getLines().zipWithIndex) {
      if (i % 1000000 == 0) println(i)
      val tokens = padding ++ line.split(" ") ++ padding
      for (i <- windowSize until tokens.length - windowSize) {
        if (wordSet(tokens(i))) {
          out.print(tokens(i))
          for (j <- (i - windowSize) until i + windowSize + 1; if j != i) out.print("\t" + normalize(tokens(j)))
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


object Stopwords {
  def asString = "a,able,about,across,after,all,almost,also,am,among,an,and,any,are,as,at,be,because,been,but,by,can,cannot,could,dear,did,do,does,either,else,ever,every,for,from,get,got,had,has,have,he,her,hers,him,his,how,however,i,if,in,into,is,it,its,just,least,let,like,likely,may,me,might,most,must,my,neither,no,nor,not,of,off,often,on,only,or,other,our,own,rather,said,say,says,she,should,since,so,some,than,that,the,their,them,then,there,these,they,this,tis,to,too,twas,us,wants,was,we,were,what,when,where,which,while,who,whom,why,will,with,would,yet,you,your"
  val all = asString.split(",").toSet
}
