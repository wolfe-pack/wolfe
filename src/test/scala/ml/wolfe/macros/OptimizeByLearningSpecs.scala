package ml.wolfe.macros

import ml.wolfe.{BruteForceOperators, Wolfe, WolfeSpec}

/**
 * @author Sebastian Riedel
 */
class OptimizeByLearningSpecs extends WolfeSpec {

  import OptimizedOperators._

  "An argmax operator" should {
    "return the argmax of the perceptron loss" in {
      import Wolfe._
      implicit val space = Range(0, 5)
      def features(i: Int) = oneHot(i)
      def model(w: Vector)(i: Int) = w dot features(i)
      def mapLL(i: Int)(w: Vector) = model(w)(i) - max { over[Int] of model(w) }
      val w = argmax { over[Vector] of mapLL(3) }
      w should be(vector(0 -> -0.2, 1 -> -0.2, 2 -> -0.2, 3 -> 0.8, 4 -> -0.2))
      //this solution arises from the fact that the MP solution at ties distributes scores across features.
    }

  }

}
