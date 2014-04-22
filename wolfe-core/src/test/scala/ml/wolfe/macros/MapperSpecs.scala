package ml.wolfe.macros

import ml.wolfe.{BruteForceOperators, Wolfe, WolfeSpec}
import ml.wolfe.Wolfe._

/**
 * @author Sebastian Riedel
 */
class MapperSpecs extends WolfeSpec {

  "An optimized map operator" should {
    "map instances to argmax predictions of a linear model when weights are independent of the instance " in {
      import OptimizedOperators._
      case class Data(x: Int, y: Int)
      implicit val ints = Range(0, 5)
      implicit def space = Wolfe.all(Data)
      def features(i: Data) = oneHot(i.x -> i.y)
      def model(w: Vector)(i: Data) = w dot features(i)
      val w: Vector = Range(0, 5).map(i => (i, i) -> 1.0).toMap
      def predict(i: Data) = argmax(space filter (_.x == i.x)) { model(w) }
      val test = ints.map(i => Data(i, i))
      val actual = map(test) { predict }
      val expected = BruteForceOperators.map(test) { predict }
      actual should be(expected)
    }

    "map instances to argmax predictions of a linear model when weights are dependent of the instance" in {
      import OptimizedOperators._
      case class Data(x: Int, y: Int)
      implicit val ints = Range(0, 5)
      implicit def space = Wolfe.all(Data)
      def features(i: Data) = oneHot(i.x -> i.y)
      def model(w: Vector)(i: Data) = w dot features(i)
      def w(d: Data): Vector = oneHot(d.x -> d.y)
      def predict(i: Data) = argmax(space filter (_.x == i.x)) { model(w(i)) }
      val test = ints.map(i => Data(i, i))
      val actual = map(test) { predict }
      val expected = BruteForceOperators.map(test) { predict }
      actual should be(expected)
    }

  }

}
