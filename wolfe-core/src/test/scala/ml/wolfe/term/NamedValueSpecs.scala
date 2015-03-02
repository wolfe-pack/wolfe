package ml.wolfe.term

import ml.wolfe.WolfeSpec

/**
 * @author riedel
 */
class NamedValueSpecs extends WolfeSpec {


  "A named value conversion" should {
    "convert an expression into a named value object where the name corresponds to the expression" in {
      val x = 5
      val named = NamedValue.toNamedValue(1 + x + 3)
      named.name should be ("(1).+(x).+(3)")
    }
  }

}
