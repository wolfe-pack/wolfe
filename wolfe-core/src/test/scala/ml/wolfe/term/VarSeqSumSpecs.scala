package ml.wolfe.term

import ml.wolfe.WolfeSpec

/**
 * @author riedel
 */
class VarSeqSumSpecs extends WolfeSpec {

  import ml.wolfe.term.TermImplicits._


  "A sum over variable length sequences" should {
    "evaluate to the sum of all its arguments when the sequence is constructed" in {
      val n = 3
      val xs = varSeqs(doubles,0,n)
      val length = xs.lengthDom.Var
      val d = doubles.Var
      val seq = VarSeq(length, IndexedSeq(d + 1.0, d * 2.0, d))
      val t = varSeqSum(seq)
      t.eval(2,1.0) should be (4.0)
    }
    "evaluate to the sum of all its arguments when the sequence is a variable" in {
      val n = 3
      val xs = varSeqs(doubles,0,n)
      val length = xs.lengthDom.Var
      val d = doubles.Var
      val x = xs.Var
      val t = varSeqSum(x)
      t.eval(IndexedSeq(1.0,2.0)) should be (3.0)
    }
    "calculate the gradient for constructed sequences" in {
      val n = 3
      val length = dom(0 until n).Var
      val d = doubles.Var
      val t = varSeqSum(VarSeq(length, IndexedSeq(d, d * d, d * d * d)))
      t.gradient(d,2,3.0) should be (7.0)
    }

  }

}
