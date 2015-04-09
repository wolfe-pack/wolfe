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
      val Bigrams = Seqs(Bools, 2)
      val BigramCounts = TypedVectors(Bigrams)
      val data = Seq(false, true, true, true)
      val bigram = Bigrams.Var
      val bigramCounts = ngramCounts(data.toConst, 2)(BigramCounts)
      val term = bigramCounts(bigram)

      term.eval(bigram := IndexedSeq(true, true)) should be(2.0)
      term.eval(bigram := IndexedSeq(false, true)) should be(1.0)
      term.eval(bigram := IndexedSeq(false, false)) should be(0.0)
    }

  }

}

