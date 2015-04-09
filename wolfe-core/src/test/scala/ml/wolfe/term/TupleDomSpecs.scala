package ml.wolfe.term

import ml.wolfe.WolfeSpec

/**
 * @author riedel
 */
class TupleDomSpecs extends WolfeSpec {

  import ml.wolfe.term.TermImplicits._

  "A tuple domain" should {
    "create variables" in {
      val x = (Bools x Bools).Var
      x.eval(x := false -> true) should be(false -> true)
    }

    "construct constants" in {
      val Pairs = Bools x Ints
      val t = Pairs.Const(true -> 2)
      t.eval() should be(true -> 2)
    }

    "provide field access" in {
      val x = (Bools x Ints).Var
      x._1.eval(x << true -> 2) should be(true)
      x._2.eval(x << true -> 2) should be(2)
    }

    "supports gradients for tuple arguments" in {
      val Pairs = Doubles x Doubles
      val x = Pairs.Var
      val term = x._1 * x._2
      val value = (1.0, 2.0)
      term.gradient(x, value) should be(2.0, 1.0)
    }

    "support pattern matching" in {
      val Tuples = (Doubles x Bools) x Doubles
      val x = Tuples.Var
      def obj(pair: Tuples.Term) = pair match {
        case Tuples.Term(Tuples.dom1.Term(a1, _), a2) => a1 + a2
      }
      val term = obj(x)
      term.eval(x <<((1.0, false), 2.0)) should be(3.0)
    }


  }

}
