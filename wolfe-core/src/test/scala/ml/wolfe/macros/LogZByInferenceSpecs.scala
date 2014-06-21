package ml.wolfe.macros

import ml.wolfe._
import ml.wolfe.fg.AndPotential

/**
 * @author Sebastian Riedel
 */
class LogZByInferenceSpecs extends WolfeSpec {

  import ml.wolfe.Wolfe._
  import ml.wolfe.macros.OptimizedOperators._

  "A logZ operator" should {

    "return the log partition function of a one-node sample space and atomic objective" in {
      val actual = logZ(0 until 5) { _.toDouble }
      val expected = BruteForceOperators.logZ(0 until 5) { _.toDouble }
//      actual should be(expected)
    }


  }

}
