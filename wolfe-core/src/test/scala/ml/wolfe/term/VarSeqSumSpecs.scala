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
      val xs = Seqs(Doubles,0,n)
      val length = xs.lengthDom.Var
      val d = Doubles.Var
      val t = sum(length)(d + 1.0, d * 2.0, d)
      t.eval(length := 2,d := 1.0) should be (4.0)
    }
    "evaluate to the sum of all its arguments when the sequence is a variable" in {
      val n = 3
      val xs = Seqs(Doubles,0,n)
      val length = xs.lengthDom.Var
      val d = Doubles.Var
      val x = xs.Var
      val t = sum(x.elements,x.length)
      t.eval(x := IndexedSeq(1.0,2.0)) should be (3.0)
    }
    "calculate the gradient for constructed sequences" in {
      val n = 3
      val length = Ints(0 until n).Var
      val d = Doubles.Var
      val t = sum(length)(d, d * d, d * d * d)
      t.gradient(d,2,3.0) should be (7.0)
    }

  }

}
