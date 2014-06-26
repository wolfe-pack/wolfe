package ml.wolfe.macros

import ml.wolfe.{Learn, BruteForceOperators, Wolfe, WolfeSpec}
import cc.factorie.optimize._

/**
 * @author Sebastian Riedel
 */
class OptimizeByLearningSpecs extends WolfeSpec {

  import OptimizedOperators._
  import Wolfe._

  "An argmax operator" should {
    "return the argmax of the MAP log-likelihood (perceptron)" in {
      implicit val space = Range(0, 5)
      def features(i: Int) = oneHot(i)
      def model(w: Vector)(i: Int) = w dot features(i)
      def mapLL(i: Int)(w: Vector) = model(w)(i) - max(space) { model(w) }
      val w = argmax(vectors) { mapLL(3) }
      w should be(Vector(0 -> -0.2, 1 -> -0.2, 2 -> -0.2, 3 -> 0.8, 4 -> -0.2))
      //this solution arises from the fact that the MP solution at ties distributes scores across features.
    }
    "return the argmin of the perceptron loss (MAP log-likelihood)" in {
      implicit val space = Range(0, 5)
      def features(i: Int) = oneHot(i)
      def model(w: Vector)(i: Int) = w dot features(i)
      def perceptronLoss(i: Int)(w: Vector) = max(space) { model(w) } - model(w)(i)
      val w = argmin(vectors) { perceptronLoss(3) }
      w should be(Vector(0 -> -0.2, 1 -> -0.2, 2 -> -0.2, 3 -> 0.8, 4 -> -0.2))
    }

    "react to different learning annotation on the objective " in {
      implicit val space = Range(0, 5)
      def features(i: Int) = oneHot(i)
      def model(w: Vector)(i: Int) = w dot features(i)
      @OptimizeByLearning(Learn.online(1,new AdaGrad()))
      def mapLLAda(i: Int)(w: Vector) = model(w)(i) - max(space) { model(w) }
      @OptimizeByLearning(Learn.online(1,new Perceptron))
      def mapLLPerceptron(i: Int)(w: Vector) = model(w)(i) - max(space) { model(w) }
      val wAda = argmax(vectors) { mapLLAda(3) }
      val wPerceptron = argmax(vectors) { mapLLPerceptron(3) }
      wAda should not be wPerceptron
    }

    "return the argmax of a sum of MAP log-likelihoods (perceptron)" in {
      case class Data(x: Int, y: Int)
      implicit val range = Range(0, 5)
      implicit def space = Wolfe.all(Data)
      val train = Range(0, 5) map (i => Data(i, i))
      def features(i: Data) = oneHot(i.x -> i.y)
      def model(w: Vector)(i: Data) = w dot features(i)
      def mapLL(i: Data)(w: Vector) = model(w)(i) - max(space where (_.x == i.x))(model(w))
      def total(w: Vector) = sum(train) { i => mapLL(i)(w) }
      val w = argmax(vectors)(total)
      for (i <- range) w(i -> i) should be(0.8)
      for (i <- range; j <- range; if i != j) w(i -> j) should be(-0.2)
    }

    "return the argmax of the log-likelihood" in {
      val n = 5
      val space = Range(0, n)
      def features(i: Int) = oneHot(i)
      def model(w: Vector)(i: Int) = w dot features(i)
      @OptimizeByLearning(Learn.batch())
      def ll(data: Seq[Int])(w: Vector) = sum(data) { i => model(w)(i) - logZ(space) { model(w) } }
      val data = Seq(0, 1, 1, 2, 3, 4)
      val w = argmax(vectors) { ll(data) }
      val mu = BruteForceOperators.expect(space)(model(w))(features)
      val empirical = sum(data)( i => features(i) * (1.0 / data.size) )
      //check if moments match!
      mu should equal (empirical)
    }


  }

}
