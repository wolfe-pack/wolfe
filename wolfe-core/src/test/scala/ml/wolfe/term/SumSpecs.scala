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
      val x = doubles.Var
      val y = doubles.Var
      val t = x + x + y
      t.eval2(1.0,2.0) should be (4.0)
    }

    "calculate its gradient" in {
      val x = doubles.Var
      val y = doubles.Var
      val t = x + x * x + y
      t.gradient2(x,2.0,2.0) should be (5.0)
    }

  }


}
