package ml.wolfe.macros

import ml.wolfe.{BruteForceOperators, Wolfe, WolfeSpec}
import ml.wolfe.Wolfe._

/**
 * @author Sebastian Riedel
 */
class MapperSpecs extends WolfeSpec {

  "An optimized map operator" should {
    "map instances to argmax predictions of a linear model " in {
      import OptimizedOperators._
      case class Data(x: Int, y: Int)
      implicit val ints = Range(0, 5)
      implicit def space = Wolfe.all(Data)
      def features(i: Data) = oneHot(i.x -> i.y)
      def model(w: Vector)(i: Data) = w dot features(i)
      val w: Vector = Range(0, 5).map(i => (i, i) -> 1.0).toMap
      def predict(i: Data) = argmax { over(space) of model(w) st (_.x == i.x) }
      val test = ints.map(i => Data(i, i))
      val actual = map { over(test) using predict }
      val expected = BruteForceOperators.map { over(test) using predict }
      actual should be (expected)
    }
  }

}
