package ml.wolfe.term

import ml.wolfe.WolfeSpec

/**
 * @author riedel
 */
class TransformerSpecs extends WolfeSpec {

  import TermImplicits._

  "A depth first transformer" should {
    "transform a term tree in a depth first fashion" in {
      val x = doubles.Var
      val y = doubles.Var
      val term = x * 2.0 + x
      val expected = y * 2.0 + y
      val transformed = Transfomer.depthFirst(term) {
        case t if t == x => y
      }
      transformed should beStringEqual (expected)
    }
  }

}
