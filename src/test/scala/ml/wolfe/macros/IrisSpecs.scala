package ml.wolfe.macros

import ml.wolfe.{MaxProduct, Wolfe, WolfeSpec}
import scala.util.Random
import ml.wolfe.macros.OptimizedWolfe._
import cc.factorie.optimize.{Perceptron, OnlineTrainer}
import ml.wolfe.Wolfe._

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
      @MaxByInference(MaxProduct(_, 1))
      def model(weights: Vector)(data: IrisData) = features(data) dot weights

      //the training loss function
      @MinByDescent(new OnlineTrainer(_, new Perceptron, 4))
      def loss(weights: Vector) = sum(train)(i => max(space)(model(weights), evidence(i)) - model(weights)(i))

      //the predictor given some observed instance
      def predict(weights: Vector)(instance: IrisData) = argmax(space)(model(weights), evidence(instance))

      val structure = MetaStructure.structure(space)

      println(structure.nodes().size)

    }


  }

}
