package ml.wolfe.term

import ml.wolfe.WolfeSpec

/**
 * @author riedel
 */
class SumSpecs extends WolfeSpec {

  import ml.wolfe.term.TermImplicits._
  import ml.wolfe.util.Math._

  "A Sum" should {
    "evaluate to the sum of its arguments" in {
      val x = Doubles.Var
      val y = Doubles.Var
      val t = x + x + y
      t.eval(1.0,2.0) should be (4.0)
    }

    "calculate its gradient" in {
      val x = Doubles.Var
      val y = Doubles.Var
      val t = x + x * x + y
      t.gradient(x,2.0,2.0) should be (5.0)
    }

  }


}
