package ml.wolfe.term

import ml.wolfe.WolfeSpec

import scala.util.Random

/**
 * @author riedel
 */
class SparsePotentialSpecs extends WolfeSpec {

  import ml.wolfe.term.TermImplicits._

  implicit val random = new Random(0)

  "Sparse Potential" should {
    "return ngram counts" in {
      implicit val NGrams = Seqs(Bools, 2)
      val data = Seq(false, true, true, true)

      val ngram = NGrams.Var

      val counts = NGram.ngramCounts(data, 2)
      val term = new SparsePotential(counts, ngram)

      term.eval(ngram := IndexedSeq(true, true)) should be(2.0)
      term.eval(ngram := IndexedSeq(false, true)) should be(1.0)
      term.eval(ngram := IndexedSeq(false, false)) should be(0.0)

    }

  }

}

