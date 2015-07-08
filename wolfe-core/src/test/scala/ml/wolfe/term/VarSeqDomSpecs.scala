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
      x.eval(x := value) should be(value)
    }

    "support element access through integer terms" in {
      val n = 3
      val x = Seqs(Bools, 0, n).Var
      val i = Ints(0 until n).Var
      val value = IndexedSeq(true, false, true)
      x(i).eval(x := value, i := 1) should be(false)
    }

    "support slice operations" in {
      val n = 4
      val x = Seqs(Bools, 0, n).Var
      implicit val sliceDom = Seqs(Bools, 2)
      val term = x.slice(1, 3)
      term.eval(x := IndexedSeq(false, true, false, true)) should be(IndexedSeq(true, false))
    }

    "support append operations" in {
      val n = 2
      val x = Seqs(Bools, 0, n).Var
      val e = Bools.Var
      implicit val appendDom = Seqs(Bools, n + 1)
      val term = x :+ e
      term.eval(x := IndexedSeq(false, true), e := true) should be(IndexedSeq(false, true, true))
    }

    "provide a hamming distance" in {
      val X = Seqs(Bools, 0, 3)
      val x1 = X.Var
      val x2 = X.Var
      X.hamming(x1,x2).eval(x1 := Vector(true,false,true), x2 := Vector(false, false, false)) should be (2.0)
    }

    "support constructing sequence terms" in {
      val n = 3
      val i = Ints(0 until n).Var
      val b = Bools.Var
      val term = SeqTerm(i)(b, !b, b)
      term.eval(i := 2, b := false) should be(IndexedSeq(false, true))
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
      val parameter = Settings(xs.toSetting(value), xs.lengthDom.toSetting(2))
      val gradient = term.createZeroInputSettings()
      val changes = new SettingChangeRecorder(gradient(0),false)
      val diff = term.differentiatorImpl(Seq(x))(parameter, Setting.cont(1.0), gradient)
      diff.differentiate()
      changes.cont.changes should be(Set(0,1,2)) //because each field has to be initialized
      changes.forget()
      diff.input(1).disc(0) = 1
      diff.differentiate()
      changes.cont.changes should be(Set(1))
    }


    "evaluate nested sequences" in {
      val n = 3
      val X = Seqs(Seqs(Doubles, 0, n), 0, n)
      val i = Ints(0 until n).Var
      val x = X.Var
      val t = x(i)
      val args = IndexedSeq(IndexedSeq(1.0, 2.0), IndexedSeq(3.0, 4.0))
      t.eval(x := args, i := 0) should be(IndexedSeq(1.0, 2.0))
      t.eval(x := args, i := 1) should be(IndexedSeq(3.0, 4.0))

      t(i).eval(x := args, i := 0) should be(1.0)
      t(i).eval(x := args, i := 1) should be(4.0)
    }

    "support argmax with length constraints" in {
      val X = Seqs(Bools, 0, 5)
      val t = argmax(X) { x => sum(x) { i => I(i) } subjectTo (x.length === 3) }
      val result = t.eval()
      result should be(IndexedSeq(true, true, true))
    }

    "support length fields in sums" in {
      val Words = List("the", "cat", "sat").toDom
      val Tags = List("DT", "NN", "VBD").toDom
      val Tokens = Pairs(Words, Tags)
      val Sentences = Seqs(Tokens, 0, 3)
      val s = Sentences.Var

      def count(s: Sentences.Term) =
        sum(0 until s.length) { t => I(s(t)._1 === Tags.Const("VBD")) }

      count(s).eval(s << IndexedSeq("the" -> "DT", "cat" -> "NN", "sat" -> "VBD")) should be(1.0)
    }

    "provide an iterable over all its elements" in {
      val Values = Seqs(Bools, 1, 2)
      val expected =
        List(Vector(false), Vector(true), Vector(false, false), Vector(false, true), Vector(true, false), Vector(true, true))

      Values.toIterable.sortBy(_.toString()) should be(expected.sortBy(_.toString()))


    }


  }

}
