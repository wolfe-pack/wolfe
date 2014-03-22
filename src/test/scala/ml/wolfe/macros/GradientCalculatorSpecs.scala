package ml.wolfe.macros

import ml.wolfe.{Wolfe, WolfeSpec}
import Wolfe._

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
    "return a constant as gradient for dot products" in {
      def f(w:Wolfe.Vector) = w dot oneHot(1, 2.0)
      val (v,g) = GradientCalculator.valueAndgradientAt(f,Wolfe.oneHot(1,5.0))
      v should be (10.0)
      g should be (oneHot(1, 2.0))
    }
  }

}
