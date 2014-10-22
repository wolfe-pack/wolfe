package ml.wolfe.macros

import ml.wolfe.{Wolfe, WolfeSpec}

/**
 * @author Sebastian Riedel
 */
class PreFactorizationChecksSpec extends WolfeSpec {

  import Wolfe._

  "A Pre-factorization check" should {
    "complain about vals instead of defs in the objective" ignore {

      def model(b:Boolean) = {
        val tmp = !b
        I(tmp)
      }

      val errors = PreFactorizationChecks.check(bools,model)
      errors should be (List((316,"""Wolfe expects tmp to be a "def" instead of a "val"""")))
    }
  }

}
