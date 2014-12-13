package ml.wolfe.macros

import ml.wolfe.{BeliefPropagation, Wolfe, WolfeSpec}
import scala.util.Random
import cc.factorie.optimize.{Perceptron, OnlineTrainer}
import ml.wolfe.util.{Iris, Evaluator}

/**
 * @author Sebastian Riedel
 */
class IrisSpecs extends WolfeSpec {

  "A Iris Model" should {
    "give reasonable performance on the IRIS dataset" in {
      import ml.wolfe.util.Iris._
      import Wolfe._
      import OptimizedOperators._
      import Library._

      //sample space of all possible Iris data values
      def space = Wolfe.all(IrisData)(Wolfe.all(IrisFeatures) x Iris.classes)

      //define what the observed part of the data is
      def observed(d: IrisData) = d.copy(irisClass = hidden)

      //feature function on data
      def features(data: IrisData) =
        oneHot('sl -> data.irisClass, data.features.sepalLength) +
        oneHot('sw -> data.irisClass, data.features.sepalWidth) +
        oneHot('pl -> data.irisClass, data.features.petalLength) +
        oneHot('pw -> data.irisClass, data.features.petalWidth)

      //the linear model
      @OptimizeByInference(BeliefPropagation(_, 1))
      def model(w: Vector)(i: IrisData) = features(i) dot w
      def predictor(w: Vector)(i: IrisData) = argmax(space filter evidence(observed)(i)) { model(w) }

      //the training loss
      @OptimizeByLearning(new OnlineTrainer(_, new Perceptron, 4))
      def loss(data: Iterable[IrisData])(w: Vector) = sum(data) { s => model(w)(predictor(w)(s)) - model(w)(s) } ////
      def learn(data: Iterable[IrisData]) = argmin(vectors) { loss(data) }

      //random generator for shuffling the data
      val random = new Random(0l)

      //the IRIS dataset
      val dataset = random.shuffle(loadIris())

      //train/test set split
      val (train, test) = dataset.splitAt(dataset.size / 2)

      val w = learn(train)

      val predictedTest = map (test) { predictor(w) }
      val predictedTrain = map (train) { predictor(w) }

      val evalTrain = Evaluator.evaluate(train, predictedTrain)(_.irisClass)
      val evalTest = Evaluator.evaluate(test, predictedTest)(_.irisClass)

      evalTrain.f1 should be(0.93 +- 0.01)
      evalTest.f1 should be(0.98 +- 0.01)


    }


  }

}
