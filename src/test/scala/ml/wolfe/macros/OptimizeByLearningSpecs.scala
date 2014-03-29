package ml.wolfe.macros

import ml.wolfe.{BruteForceOperators, Wolfe, WolfeSpec}

/**
 * @author Sebastian Riedel
 */
class OptimizeByLearningSpecs extends WolfeSpec {

  import OptimizedOperators._

  "An argmax operator" should {
    "return the argmax of perceptron loss" in {
      import Wolfe._
      implicit val space = Range(0, 5)
      def features(i:Int) = oneHot(i)
      def model(w:Vector)(i:Int) = w dot features(i)
      def mapLL(i:Int)(w:Vector) = model(w)(i) - max { over[Int] of model(w) }
//      val w = argmax { over[Vector] of mapLL(3)}
//      val actual = argmax { over(Range(0, 5)) of (_.toDouble) }
//      val expected = BruteForceOperators.argmax { over(Range(0, 5)) of (_.toDouble) }
//      actual should be(expected)
    }

  }

}
