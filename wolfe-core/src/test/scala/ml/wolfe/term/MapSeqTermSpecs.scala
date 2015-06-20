package ml.wolfe.term

import ml.wolfe.WolfeSpec

/**
 * @author sameer
 */
class MapSeqTermSpecs extends WolfeSpec {

  import ml.wolfe.term.TermImplicits._

  "A variable sequence length identity map" should {
    "evaluate to itself" ignore {
      val n = 3
      val indices = Seqs(Bools, 0, n).Var
      val term = map(indices) { v => v }
      term.eval(indices := IndexedSeq(false, true)) should be(IndexedSeq(false, true))
      term.eval(indices := IndexedSeq(false, true, true)) should be(IndexedSeq(false, true, true))
    }
  }

  "A variable sequence length indicator map" should {
    "evaluate to itself as doubles" ignore {
      val n = 3
      val indices = Seqs(Bools, 0, n).Var
      val term = map(indices) { v => I(v) }
      term.eval(indices := IndexedSeq(false, true)) should be(IndexedSeq(0.0, 1.0))
      term.eval(indices := IndexedSeq(false, true, true)) should be(IndexedSeq(0.0, 1.0, 1.0))
    }
  }

}

