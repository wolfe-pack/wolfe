package scalapplcodefest.macros

import scalapplcodefest.Wolfe
import Wolfe._
import scala.util.Random
import scalapplcodefest.Wolfe.Objective.{Perceptron, Differentiable}
import scalapplcodefest.newExamples.Iris

/**
 * @author Sebastian Riedel
 */
object TestIris {

  import Iris._

  def main(args: Array[String]) {
    //random generator for shuffling the data
    val random = new Random(0l)

    //the IRIS dataset
    val dataset = random.shuffle(Iris.loadIris())

    //train/test set split
    val (train, test) = dataset.splitAt(dataset.size / 2)

    //the set of all possible dates
    def sampleSpace = all2(Data)(c(all2(Observed)(c(doubles, doubles, doubles, doubles)), classes))

    //joint feature function on data (x,y)
    def features(data: Data) =
      oneHot('sl -> data.y, data.x.sepalLength) +
      oneHot('sw -> data.y, data.x.sepalWidth) +
      oneHot('pl -> data.y, data.x.petalLength) +
      oneHot('pw -> data.y, data.x.petalWidth)

    //the linear model
    def model(weights: Vector)(data: Data) = features(data) dot weights

    //the total training perceptron loss of the model given the weights
    @Differentiable(Perceptron, 5)
    def loss(weights: Vector) = sum(train)(_ => true)(data => max(sampleSpace)(_.x == data.x)(model(weights)) - model(weights)(data))

    //the learned weights that minimize the perceptron loss
    val learned = argmin(vectors)(_ => true)(loss)



  }
}
