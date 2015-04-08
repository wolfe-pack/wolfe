package ml.wolfe.term

import ml.wolfe.WolfeSpec

import scala.util.Random

/**
 * @author riedel
 */
class NGramCountSpecs extends WolfeSpec {

  import ml.wolfe.term.TermImplicits._

  implicit val random = new Random(0)

  "NGram Counts" should {
    "count the number of ngrams in a dataset" in {
      import NGramCountsHelper._
      val BigramCounts = TypedVectors(Seqs(Bools, 2))
      val data = Seq(false, true, true, true)
      val bigram = BigramCounts.argDom.Var
      val bigramCounts = ngramCounts(data.toConst,2)(BigramCounts)
      val term = bigramCounts(bigram)

      println(term.eval(bigram := IndexedSeq(true, true)))





//      val counts = NGram.ngramCounts(data, 2)
//      val term = new SparsePotential(counts, ngram)
//
//      term.eval(ngram := IndexedSeq(true, true)) should be(2.0)
//      term.eval(ngram := IndexedSeq(false, true)) should be(1.0)
//      term.eval(ngram := IndexedSeq(false, false)) should be(0.0)

    }

  }

}

