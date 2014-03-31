package ml.wolfe.macros

import ml.wolfe.{MaxProduct, Wolfe, WolfeSpec}
import scala.util.Random
import cc.factorie.optimize.{Perceptron, OnlineTrainer}
import ml.wolfe.util.Evaluator

/**
 * @author Sebastian Riedel
 */
class IrisSpecs extends WolfeSpec {

  "A Iris Model" should {
    "give reasonable performance on the IRIS dataset " in {
      import ml.wolfe.util.Iris._
      import Wolfe._
      import OptimizedOperators._

      //random generator for shuffling the data
      val random = new Random(0l)

      //the IRIS dataset
      val dataset = random.shuffle(loadIris())

      //train/test set split
      val (train, test) = dataset.splitAt(dataset.size / 2)

      //sample space of all possible Iris data values
      def space = Wolfe.all(IrisData)

      //define what the observed part of the data is
      def observed(d: IrisData) = d.copy(irisClass = hide[Label])

      //conditioning on the observation of given training instance
      def evidence(instance: IrisData)(sample: IrisData) = observed(sample) == observed(instance)

      //feature function on data
      def features(data: IrisData) =
        oneHot('sl -> data.irisClass, data.sepalLength) +
        oneHot('sw -> data.irisClass, data.sepalWidth) +
        oneHot('pl -> data.irisClass, data.petalLength) +
        oneHot('pw -> data.irisClass, data.petalWidth)

      //the linear model
      @OptimizeByInference(MaxProduct(_, 1))
      def model(w: Vector)(i: IrisData) = features(i) dot w

      //the per instance training loss
      def perceptronLoss(w: Vector)(i: IrisData): Double = max { over(space) of model(w) st evidence(i) } - model(w)(i)

      //the training loss
      @OptimizeByLearning(new OnlineTrainer(_, new Perceptron, 4))
      def loss(w: Vector) = sum { over(train) of perceptronLoss(w) }

      val w = argmin { over[Vector] of loss }

      //the predictor given some observed instance
      def predict(i: IrisData) = argmax(over(space) of model(w) st evidence(i))

      val predictedTest = map { over(test) using predict }
      val predictedTrain = map { over(train) using predict }

      val evalTrain = Evaluator.evaluate(train, predictedTrain)(_.irisClass)
      val evalTest = Evaluator.evaluate(test, predictedTest)(_.irisClass)

      evalTrain.f1 should be(0.93 +- 0.01)
      evalTest.f1 should be(0.98 +- 0.01)


    }


  }

}
