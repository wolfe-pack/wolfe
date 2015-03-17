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
      x.eval(false -> true) should be (false -> true)
    }

    "construct constants" in {
      val Pairs = Bools x Ints
      val t = Pairs.Const(true -> 2)
      t.eval() should be (true -> 2)
    }

    "provide field access" in {
      val x = (Bools x Ints).Var
      x._1 (x << true -> 2) should be (true)
      x._2 (x << true -> 2) should be (2)
    }

    "supports gradients for tuple arguments" in {
      val Pairs = Doubles x Doubles
      val x = Pairs.Var
      val term = x._1 * x._2
      val value = (1.0, 2.0)
      term.gradient(x, value) should be(2.0,1.0)
    }



  }

}
