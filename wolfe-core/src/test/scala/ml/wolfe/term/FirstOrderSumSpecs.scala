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
      val index = seqs(bools,0,n).Var
      val term = sum(index) { v => I(v)}
      term.eval(IndexedSeq(false,true)) should be (1.0)
      term.eval(IndexedSeq(false,true,true)) should be (2.0)
    }
    "calculate its gradient" in {
      val n = 3
      val index = seqs(dom(0 until n),0,n).Var
      val x = seqs(doubles,n,n).Var
      val term = sum(index) { i => x(i) * x(i) }
      term.gradient(x,IndexedSeq(1),IndexedSeq(1.0,2.0,3.0)) should be (IndexedSeq(0.0,4.0,0.0))
      term.gradient(x,IndexedSeq(1,2),IndexedSeq(1.0,2.0,3.0)) should be (IndexedSeq(0.0,4.0,6.0))
      term.gradient(x,IndexedSeq(1,1),IndexedSeq(1.0,2.0,3.0)) should be (IndexedSeq(0.0,8.0,0.0))
    }


  }

}
