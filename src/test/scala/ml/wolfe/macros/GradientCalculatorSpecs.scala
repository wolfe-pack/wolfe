package ml.wolfe.macros

import ml.wolfe.{Wolfe, WolfeSpec}
import Wolfe._

/**
 * @author Sebastian Riedel
 */
class GradientCalculatorSpecs extends WolfeSpec {

  "A gradient calculator" should {
    "return 0 gradient for a constant function " in {
      def f(w: Wolfe.Vector) = 5.0
      val (v, g) = GradientCalculator.valueAndgradientAt(f, oneHot(1, 5.0))
      v should be(5.0)
      g should be(Wolfe.VectorZero)
    }
    "return a constant as gradient for dot products" in {
      def f(w: Wolfe.Vector) = w dot oneHot(1, 2.0)
      val (v, g) = GradientCalculator.valueAndgradientAt(f, oneHot(1, 5.0))
      v should be(10.0)
      g should be(oneHot(1, 2.0))
    }
    "return the difference of gradients for a difference of functions" in {
      def f(w: Wolfe.Vector) = (w dot oneHot(1, 2.0)) - (w dot oneHot(1, 1.0))
      val (v, g) = GradientCalculator.valueAndgradientAt(f, oneHot(1, 5.0))
      v should be(5.0)
      g should be(oneHot(1, 1.0))
    }
    "return the sum of gradients for a sum of functions" in {
      def f(w: Wolfe.Vector) = (w dot oneHot(1, 2.0)) + (w dot oneHot(1, 1.0))
      val (v, g) = GradientCalculator.valueAndgradientAt(f, oneHot(1, 5.0))
      v should be(15.0)
      g should be(oneHot(1, 3.0))
    }
    "return a subgradient of a max expression" in {
      import OptimizedWolfe._
      case class Data(x: Symbol, y: Symbol)
      val space = Wolfe.all(Data)(c(Seq('X1, 'X2), Seq('Y1, 'Y2, 'Y3)))
      def f(w: Vector) = max(space)(_.x == 'X2)(d => oneHot(d.x -> d.y, 1.0) dot w)
      val (v, g) = GradientCalculator.valueAndgradientAt(f, oneHot('X2 -> 'Y3, 2.0))
      g should be(oneHot('X2 -> 'Y3,1.0))
      v should be(2.0)

    }
  }

}
