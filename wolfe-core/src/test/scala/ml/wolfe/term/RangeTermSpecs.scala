package ml.wolfe.term

import ml.wolfe.WolfeSpec
import ml.wolfe.util.Math._

/**
 * @author rockt
 */
class RangeTermSpecs extends WolfeSpec {
  import ml.wolfe.term.TermImplicits._

  "A range term" should {
    "evaluate to a range" in {
      val end = Ints(3 until 5).Var
      val start = Ints(0 until 2).Var
      val range = start until end
      (range | start << 1 | end << 4).eval2() should be (Range(1,4))
    }
  }
}
