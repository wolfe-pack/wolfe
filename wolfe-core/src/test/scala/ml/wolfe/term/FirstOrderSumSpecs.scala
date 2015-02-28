package ml.wolfe.term

import ml.wolfe.WolfeSpec

/**
 * @author riedel
 */
class FirstOrderSumSpecs extends WolfeSpec {

  import ml.wolfe.term.TermImplicits._


  "A variable sequence length first order sum" should {
    "evaluate to the sum of its arguments" in {
      val n = 3
      val indices = seqs(bools,0,n).Var
      val term = sum(indices) { v => I(v)}
      term.eval2(IndexedSeq(false,true)) should be (1.0)
      term.eval2(IndexedSeq(false,true,true)) should be (2.0)
    }

    "evaluate nested sums" in {
      val n = 3
      val indices = seqs(seqs(bools,0,n),0,n).Var
      val term = sum(indices) {i => sum(i) {j => I(j)}}
      val args = IndexedSeq(IndexedSeq(true,true),IndexedSeq(false,true))
      term.eval2(args) should be (3.0)
    }

    "calculate its gradient" in {
      val n = 3
      val indices = seqs(dom(0 until n),0,n).Var
      val x = seqs(doubles,n,n).Var
      val term = sum(indices) { i => x(i) * x(i) }
      val seq = IndexedSeq(1.0, 2.0, 3.0)
      term.gradient2(x,IndexedSeq(1),seq) should be (IndexedSeq(0.0,4.0,0.0))
      term.gradient2(x,IndexedSeq(1,2),seq) should be (IndexedSeq(0.0,4.0,6.0))
      term.gradient2(x,IndexedSeq(1,1),seq) should be (IndexedSeq(0.0,8.0,0.0))
    }

  }

}
