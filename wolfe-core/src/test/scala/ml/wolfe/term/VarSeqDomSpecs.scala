package ml.wolfe.term

import ml.wolfe.WolfeSpec

/**
 * @author riedel
 */
class VarSeqDomSpecs extends WolfeSpec {

  import ml.wolfe.term.TermImplicits._

  "A variable length sequence domain" should {
    "create variables" in {
      val xs = Seqs(Bools, 0, 5)
      val x = xs.Var
      val value = IndexedSeq(true, false, true)
      x.eval(value) should be(value)
    }

    "support element access through integer terms" in {
      val n = 3
      val x = Seqs(Bools, 0, n).Var
      val i = Ints(0 until n).Var
      val value = IndexedSeq(true, false, true)
      x(i).eval(value, 1) should be(false)
    }

    "support constructing sequence terms" in {
      val n = 3
      val i = Ints(0 until n).Var
      val b = Bools.Var
      val term = SeqTerm(i)(b, !b, b)
      term.eval(2, false) should be(IndexedSeq(false, true))
    }

    "supports gradients for sequence arguments" in {
      val n = 3
      val xs = Seqs(Doubles, 0, n)
      val x = xs.Var
      val i = xs.lengthDom.Var
      val value = IndexedSeq(1.0, 2.0, 3.0)
      val term = x(i) * x(i + 1)
      term.gradient(x, value, 1) should be(IndexedSeq(0.0, 3.0, 2.0))
    }

    "should make sparse updates when calculating the gradient" in {
      val n = 3
      val xs = Seqs(Doubles, 0, n)
      val x = xs.Var
      val i = xs.lengthDom.Var
      val value = IndexedSeq(1.0, 2.0, 3.0)
      val term = x(i) * x(i)
      val parameter = Settings(xs.toSetting(value),xs.lengthDom.toSetting(2))
      val gradient = term.createZeroSettings()
      val diff = term.differentiatorImpl(Seq(x))(parameter,Setting.cont(1.0),gradient)
      gradient(0).recordChangedOffsets = true
      diff.differentiate()
      gradient(0).cont.changed() should be (Set(2))
    }


    "evaluate nested sequences" in {
      val n = 3
      val xs = Seqs(Seqs(Doubles, 0, n), 0, n)
      val i = Ints(0 until n).Var
      val x = xs.Var
      val t = x(i)
      val args = IndexedSeq(IndexedSeq(1.0, 2.0), IndexedSeq(3.0, 4.0))
      t.eval(args, 0) should be(IndexedSeq(1.0, 2.0))
      t.eval(args, 1) should be(IndexedSeq(3.0, 4.0))

      t(i).eval(args, 0) should be(1.0)
      t(i).eval(args, 1) should be(4.0)
    }

  }

}
