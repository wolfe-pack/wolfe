package ml.wolfe.macros

import ml.wolfe.{BruteForceOperators, WolfeSpec}

/**
 * @author Sebastian Riedel
 */
class ExpectationSpecs extends WolfeSpec {

  import ml.wolfe.Wolfe._

  "return the expectations of a one-node sample space and atomic objective " in {
    val actual = OptimizedOperators.expect(0 until 5) { _.toDouble } {oneHot(_)}
    val expected = BruteForceOperators.expect(0 until 5) { _.toDouble } {oneHot(_)}
    actual should equal (expected)
  }

}
