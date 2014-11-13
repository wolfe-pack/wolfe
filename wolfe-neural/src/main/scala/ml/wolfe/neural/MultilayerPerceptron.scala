package ml.wolfe.neural

import breeze.linalg.{DenseVector, DenseMatrix}
import ml.wolfe.neural.math.ActivationFunctions

/**
 * Created by mbosnjak on 11/6/14.
 */

// extends iterable of layers ? (check of last layer)
// last layer needs to be non-hidden ?
// parameters: sequence of layers  +  final layer ?
class MultiLayerPerceptron(val layers: Seq[NetworkLayer]) {
  require(layers.head.isInstanceOf[HiddenLayer], "Input layer must be of type HiddenLayer (for now.)")
  require(layers.last.isInstanceOf[OutputLayer], "Output layer must be of type OutputLayer.")

  for (i <- 0 until layers.size - 2) {
    val slice = layers.view(i, i + 1)
    require(slice(0).outputSize == slice(1).inputSize, "Layers %d and %d don't agree in output - input sizes".format(i, i+1))
  }

  def predict(input: DenseMatrix[Double]): DenseVector[Double] = {
    var cmat = input
    for (l <- layers) {

    }
    ???
  }


  // need to define a total cost, output + regularization?
}

object MultiLayerPerceptronPlayground extends App {



//
//  val x = DenseMatrix((0.1, 0.1), (0.2,0.1), (0.3, 0.5))
//
//  val W1 = DenseMatrix((1.0, 2.0, 3.0),(2.0, 2.0, 3.0))
//  val b1 = DenseVector(2.0, 2.0, 6.0)
//
//  val W2 = DenseMatrix((1.0, 1.0), (2.0, 2.0), (2.0, 1.0))
//  val b2 = DenseVector(3.0, 1.0)
//
//  val W3 = DenseMatrix((1.0, 3.0), (2.0, 2.0))
//  val b3 = DenseVector(1.0, 1.0)
//
//

//
//
//  val l1 = new HiddenLayer(x, 3, W1, b1, ActivationFunctions.sigmoid)
//
//  val l2 = new HiddenLayer(l1.output, 2, W2, b2, ActivationFunctions.sigmoid)
//  val l3 = new LogisticRegression(l2.output, W3, b3)
//
//
//  val mlp = new MultiLayerPerceptron(Seq(l1, l2, l3))
//
//
//  println("Hello MultiLayer Perceptron!")
}
