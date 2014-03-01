package scalapplcodefest.newExamples

import scalapplcodefest.sbt.Compile
import scalapplcodefest.Wolfe
import scala.util.Random
import scalapplcodefest.Wolfe.Objective.{Perceptron, Differentiable}
import scalapplcodefest.util.Evaluator

/**
 * This example shows how to train and run a linear classifier on the IRIS dataset. This example intentionally uses
 * very little syntactic sugar or DSL code. Its purpose is to show the math behind a classifier.
 *
 * @author Sebastian Riedel
 */
@Compile
class IrisShort extends (() => Unit) {

  import scalapplcodefest.ShortMathDSL._
  import Iris._

  def apply() {
    //random generator for shuffling the data
    val random = new Random(0l)

    //the IRIS dataset
    val dataset = random.shuffle(Iris.loadIris())

    //train/test set split
    val (train, test) = dataset.splitAt(dataset.size / 2)

    //the set of all possible dates
    implicit def sampleSpace = all2(Data)(c(all2(Observed)(c(doubles, doubles, doubles, doubles)), classes))

    //joint feature function on data (x,y)
    def features(data: Data) = oneHot('sl -> data.y, data.x.sepalLength) +
      oneHot('sw -> data.y, data.x.sepalWidth) +
      oneHot('pl -> data.y, data.x.petalLength) +
      oneHot('pw -> data.y, data.x.petalWidth)

    //the linear model
    def model(weights: Vector)(data: Data) = features(data) dot weights

    //the total training perceptron loss of the model given the weights
    @Differentiable(Perceptron, 5)
    def loss(weights: Vector) = sum(train) {max(model(weights)) - model(weights)(_)}

    //the learned weights that minimize the perceptron loss
    val learned = argmin(loss)

    //the predictor using the learned weights
    def predict(data: Data) = argmax((_: Data).x == data.x) {model(learned)}

    //apply the predictor to each instance of the test set.
    val predicted = test.map(predict)

    println(learned)

    println(Evaluator.evaluate(test, predicted)(_.y))

  }


}
