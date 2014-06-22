package ml.wolfe.macros

import ml.wolfe.{Wolfe, WolfeSpec}
import Wolfe._
import org.scalautils.Equality

/**
 * @author Sebastian Riedel
 */
class GradientCalculatorSpecs extends WolfeSpec {

  implicit val vectorEq = new Equality[Vector] {
    def areEqual(a: Wolfe.Vector, b: Any) = b match {
      case v:Wolfe.Vector => a.keySet == v.keySet && a.keySet.forall(k => math.abs(a(k) - v(k)) < 0.0001)
      case _ => false
    }
  }

  "A gradient calculator" should {
    "return 0 gradient for a constant function" in {
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
    "return a constant as gradient for dot products with a large sum" in {
      def vector =
        oneHot(0, 0.0) +
        oneHot(1, 1.0) +
        oneHot(2, 2.0) +
        oneHot(3, 3.0) +
        oneHot(4, 4.0)
      def f(w: Wolfe.Vector) = w dot vector
      val (v, g) = GradientCalculator.valueAndgradientAt(f, oneHot(1, 5.0))
      v should be(oneHot(1, 5.0) dot vector)
      g should be(vector)
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
      case class Data(x: Symbol, y: Symbol)
      def space = Wolfe.all(Data)(c(Seq('X1, 'X2), Seq('Y1, 'Y2, 'Y3)))
      def f(w: Vector) = space.filter(_.x == 'X2).map(d => oneHot(d.x -> d.y, 1.0) dot w).max
      val (v, g) = GradientCalculator.valueAndgradientAt(f, oneHot('X2 -> 'Y3, 2.0))
      g should be(oneHot('X2 -> 'Y3, 1.0))
      v should be(2.0)
    }

    "return a subgradient of a max expression using max operator" in {
      import OptimizedOperators._
      case class Data(x: Symbol, y: Symbol)
      def space = Wolfe.all(Data)(c(Seq('X1, 'X2), Seq('Y1, 'Y2, 'Y3)))
      def f(w: Vector) = max(space where (_.x == 'X2))(d => oneHot(d.x -> d.y, 1.0) dot w)
      val (v, g) = GradientCalculator.valueAndgradientAt(f, oneHot('X2 -> 'Y3, 2.0))
      g should be(oneHot('X2 -> 'Y3, 1.0))
      v should be(2.0)
    }



    "return a subgradient of a max expression formulated using argmax " in {
      import OptimizedOperators._
      case class Data(x: Symbol, y: Symbol)
      def space = Wolfe.all(Data)(c(Seq('X1, 'X2), Seq('Y1, 'Y2, 'Y3)))
      def model(w: Vector)(d: Data) = oneHot(d.x -> d.y, 1.0) dot w
      def f(w: Vector) = model(w)(argmax(space)(model(w)))
      val (v, g) = GradientCalculator.valueAndgradientAt(f, oneHot('X2 -> 'Y3, 2.0))
      g should be(oneHot('X2 -> 'Y3, 1.0))
      v should be(2.0)
    }

    "return the gradient of a log partition function expression using logZ operator" in {
      import OptimizedOperators._
      def space = 0 until 4
      def f(w: Vector) = logZ(space)(d => oneHot(d, 1.0) dot w)
      val (v, g) = GradientCalculator.valueAndgradientAt(f, oneHot(1, math.log(2.0)))
      val Z = 1.0 + 2.0 + 1.0 + 1.0
      val expected = Vector(0 -> 1 / Z, 1 -> 2 / Z, 2 -> 1 / Z, 3 -> 1 / Z)
      g should be(expected)
      v should be(math.log(Z) +- 0.0001)
    }

    "return the gradient of a log partition function expression of a markov chain using logZ operator" in {
      import OptimizedOperators._
      def space = seqs(4, bools)
      def feat(seq: Seq[Boolean]) = sum(0 until seq.size - 1) { i => oneHot(seq(i) -> seq(i + 1)) }
      def model(w: Vector)(seq: Seq[Boolean]) = feat(seq) dot w
      val weights = Vector((false, false) -> 1.0, (false, true) -> 2.0, (true, false) -> -3.0, (true, true) -> 0.0)
      def f(w: Vector) = logZ(space) { model(w) }
      val (v, g) = GradientCalculator.valueAndgradientAt(f, weights)
      val Z = (space map (s => math.exp(model(weights)(s)))).sum
      val expectations = (space map (s => feat(s) * (math.exp(model(weights)(s)) / Z))).sum
      v should be(math.log(Z) +- 0.0001)
      g should equal (expectations)

    }


  }

}
