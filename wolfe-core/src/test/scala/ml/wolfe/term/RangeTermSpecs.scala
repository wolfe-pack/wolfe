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
      val start = Ints(0 until 2).Var
      val end = Ints(3 until 5).Var
      val range = start until end
      range(start << 1, end << 4) should be (Range(1,4))
    }
    "have a precise domain" ignore {
      val start = Ints(0 until 2).Var
      val end = Ints(4 until 6).Var
      val range = start until end - 1
      range.domain.minLength should be (2) //smallest = 1 until 3 // length == 2
      range.domain.maxLength should be (4) //largest = 0 until 4 // length == 4
      range.domain.elementDom.values should be (0 until 4)

    }

  }
}
