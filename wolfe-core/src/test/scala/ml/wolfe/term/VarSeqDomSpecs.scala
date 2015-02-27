package ml.wolfe.term

import ml.wolfe.WolfeSpec

/**
 * @author riedel
 */
class VarSeqDomSpecs extends WolfeSpec {

  import ml.wolfe.term.TermImplicits._

  "A variable length sequence domain" should {
    "create variables" in {
      val xs = seqs(bools,0,5)
      val x = xs.Var
      val value = IndexedSeq(true,false,true)
      x.eval(value) should be (value)
    }

    "support element access through integer terms" in {
      val n = 3
      val x = seqs(bools,0,n).Var
      val i = dom(0 until n).Var
      val value = IndexedSeq(true,false,true)
      x(i).eval(value,1) should be (false)
    }

    "support constructing sequence terms" in {
      val n = 3
      val i = dom(0 until n).Var
      val b = bools.Var
      val term = VarSeq(i,IndexedSeq(b,!b,b))
      term.eval(2,false) should be (IndexedSeq(false,true))
    }

    "supports gradients for sequence arguments" in {
      val n = 3
      val xs = seqs(doubles,0,n)
      val x = xs.Var
      val i = xs.lengthDom.Var
      val value = IndexedSeq(1.0,2.0,3.0)
      val term = x(i) * x(i)
      term.gradient(x,value,1) should be (IndexedSeq(0.0,4.0,0.0))
    }

  }

}
