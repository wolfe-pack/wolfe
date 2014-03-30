package ml.wolfe.macros

import ml.wolfe.{BruteForceOperators, Wolfe, WolfeSpec}
import cc.factorie.optimize.{Perceptron, AdaGrad, OnlineTrainer}

/**
 * @author Sebastian Riedel
 */
class OptimizeByLearningSpecs extends WolfeSpec {

  import OptimizedOperators._

  "An argmax operator" should {
    "return the argmax of the MAP log-likelihood (perceptron)" in {
      import Wolfe._
      implicit val space = Range(0, 5)
      def features(i: Int) = oneHot(i)
      def model(w: Vector)(i: Int) = w dot features(i)
      def mapLL(i: Int)(w: Vector) = model(w)(i) - max { over[Int] of model(w) }
      val w = argmax { over[Vector] of mapLL(3) }
      w should be(vector(0 -> -0.2, 1 -> -0.2, 2 -> -0.2, 3 -> 0.8, 4 -> -0.2))
      //this solution arises from the fact that the MP solution at ties distributes scores across features.
    }
    "return the argmin of the perceptron loss  MAP log-likelihood)" in {
      import Wolfe._
      implicit val space = Range(0, 5)
      def features(i: Int) = oneHot(i)
      def model(w: Vector)(i: Int) = w dot features(i)
      def mapLL(i: Int)(w: Vector) = max { over[Int] of model(w) } - model(w)(i)
      val w = argmin { over[Vector] of mapLL(3) }
      w should be(vector(0 -> -0.2, 1 -> -0.2, 2 -> -0.2, 3 -> 0.8, 4 -> -0.2))
      //this solution arises from the fact that the MP solution at ties distributes scores across features.
    }

    "react to different learning annotation on the objective " in {
      import Wolfe._
      implicit val space = Range(0, 5)
      def features(i: Int) = oneHot(i)
      def model(w: Vector)(i: Int) = w dot features(i)
      @OptimizeByLearning(new OnlineTrainer(_, new AdaGrad(),1))
      def mapLLAda(i: Int)(w: Vector) = model(w)(i) - max { over[Int] of model(w) }
      @OptimizeByLearning(new OnlineTrainer(_, new Perceptron,1))
      def mapLLPerceptron(i: Int)(w: Vector) = model(w)(i) - max { over[Int] of model(w) }
      val wAda = argmax { over[Vector] of mapLLAda(3) }
      val wPerceptron = argmax { over[Vector] of mapLLPerceptron(3) }
      wAda should not be wPerceptron
    }

    "return the argmax of a sum of MAP log-likelihoods (perceptron)" in {
      import Wolfe._
      case class Data(x: Int, y: Int)
      implicit val range = Range(0, 5)
      implicit def space = Wolfe.all(Data)
      val train = Range(0, 5) map (i => Data(i, i))
      def features(i: Data) = oneHot(i.x -> i.y)
      def model(w: Vector)(i: Data) = w dot features(i)
      def mapLL(i: Data)(w: Vector) = model(w)(i) - max { over[Data] of model(w) st (_.x == i.x) }
      def total(w: Vector) = sum { over(train) of { i => mapLL(i)(w) } }
      val w = argmax { over[Vector] of total }
      for (i <- range) w(i -> i) should be(0.8)
      for (i <- range; j <- range; if i != j) w(i -> j) should be(-0.2)
    }


  }

}
