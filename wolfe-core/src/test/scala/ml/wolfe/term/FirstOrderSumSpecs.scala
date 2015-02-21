package ml.wolfe.term

import ml.wolfe.WolfeSpec

/**
 * @author riedel
 */
class FirstOrderSumSpecs extends WolfeSpec {

  import ml.wolfe.term.TermImplicits._

  "A first order sum" should {
    "evaluate to the sum of all its arguments" in {
      val n = 2
      val X = seqs(doubles,n)
      val x = X.variable("x")
      val t = sum(dom(0 until n)){i => x(i) * i}
      t.eval(IndexedSeq(1.0,2.0)) should be (2.0)
    }

    "calculate its gradient" in {
      val n = 4
      val X = seqs(doubles,n)
      val x = X.Var
      val t = sum(dom(0 until n)) {i => x(i) * x(i) }
      val args = (0 until n).map(_.toDouble)
      t.gradient(x,args) should be (for (i <- 0 until n) yield 2.0 * i)
    }

  }

}
