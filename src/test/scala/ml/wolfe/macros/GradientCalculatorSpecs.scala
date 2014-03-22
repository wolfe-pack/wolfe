package ml.wolfe.macros

import ml.wolfe.{Wolfe, WolfeSpec}

/**
 * @author Sebastian Riedel
 */
class GradientCalculatorSpecs extends WolfeSpec {

  "A gradient calculator" should {
    "return 0 gradient for a constant function" in {
      def f(w:Wolfe.Vector) = 5.0
      val (v,g) = GradientCalculator.valueAndgradientAt(f,Wolfe.oneHot("k",5.0))
      v should be (5.0)
      g should be (Wolfe.VectorZero)
    }
  }

}
