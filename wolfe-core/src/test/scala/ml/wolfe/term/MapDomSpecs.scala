package ml.wolfe.term

import ml.wolfe.WolfeSpec

/**
 * @author riedel
 */
class MapDomSpecs extends WolfeSpec {

  import ml.wolfe.term.TermImplicits._

  "A map domain" should {
    "create variables" in {
      val M = Maps(Bools, Ints)
      val m = M.Var
      m.eval(m << Map(false -> 1, true -> 2)) should be(Map(false -> 1, true -> 2))
      m.eval(m << Map(false -> 1)) should be(Map(false -> 1))
    }

    "access map elements of a variable" in {
      val M = Maps(Bools, Ints)
      val m = M.Var
      val k = Bools.Var
      val t = m(k)
      t.eval(m << Map(false -> 5), k << false) should be(5)
    }

    "access map elements of a constant" in {
      implicit val M = Maps(Bools, Ints)
      val m = Map(false -> 1, true -> 2).toConst
      val k = Bools.Var
      m(k).eval(k << true) should be(2)
    }

    "supports gradients for map values" in {
      val M = Maps(Bools, Doubles)
      val m = M.Var
      val k = Bools.Var
      val term = m(k) * m(k) * 2.0 + m(!k) * m(!k) * 3.0
      term.gradient(m, Map(true -> 1.0, false -> 2.0), true) should be(Map(true -> 4.0, false -> 12.0))
    }

    "support binary arguments" in {
      val k = Bools.Var
      val M = Maps(Bools, Bools, Doubles)
      val m = M.Var
      val t = m(k,k)
      t.eval(m << Map((true,true)->2.0), k << true) should be (2.0)
    }

    "provide an iterable view on the domain" in {
      val M = Maps(Bools, Bools)
      val expected = for (k1 <- Bools; k2 <- Bools; r1 <- Bools; r2 <- Bools; size <- 0 to 2) yield {
        Map(k1 -> r1, k2 -> r2).take(size)
      }
      M.toIterable.sortBy(_.keys) should be (expected.toList.distinct.sortBy(_.keys))
    }

    //
    //    "should make sparse updates when calculating the gradient" in {
    //      val n = 3
    //      val xs = Seqs(Doubles, 0, n)
    //      val x = xs.Var
    //      val i = xs.lengthDom.Var
    //      val value = IndexedSeq(1.0, 2.0, 3.0)
    //      val term = x(i) * x(i)
    //      val parameter = Settings(xs.toSetting(value), xs.lengthDom.toSetting(2))
    //      val gradient = term.createZeroInputSettings()
    //      val diff = term.differentiatorImpl(Seq(x))(parameter, Setting.cont(1.0), gradient)
    //      gradient(0).recordChangedOffsets = true
    //      diff.differentiate()
    //      gradient(0).cont.changed() should be(Set(2))
    //    }
    //
    //
    //    "evaluate nested sequences" in {
    //      val n = 3
    //      val X = Seqs(Seqs(Doubles, 0, n), 0, n)
    //      val i = Ints(0 until n).Var
    //      val x = X.Var
    //      val t = x(i)
    //      val args = IndexedSeq(IndexedSeq(1.0, 2.0), IndexedSeq(3.0, 4.0))
    //      t.eval(args, 0) should be(IndexedSeq(1.0, 2.0))
    //      t.eval(args, 1) should be(IndexedSeq(3.0, 4.0))
    //
    //      t(i).eval(args, 0) should be(1.0)
    //      t(i).eval(args, 1) should be(4.0)
    //    }
    //
    //    "support argmax with length constraints" in {
    //      val X = Seqs(Bools, 0, 5)
    //      val t = argmax(X) { x => sum(x) { i => I(i)} subjectTo (x.length === 3)}
    //      val result = t.eval()
    //      result should be(IndexedSeq(true, true, true))
    //    }

  }

}
