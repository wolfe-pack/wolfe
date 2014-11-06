package ml.wolfe.neural

import breeze.linalg.{DenseVector, DenseMatrix}

/**
 * Created by mbosnjak on 11/6/14.
 */

// extends iterable of layers ? (check of last layer)
// last layer needs to be non-hidden ?
// parameters: sequence of layers  +  final layer ?
class MultiLayerPerceptron(layers: Seq[NetworkLayer]) {
  assert(layers.head.isInstanceOf[HiddenLayer], "Input layer must be of type HiddenLayer (for now.)")
  assert(layers.last.isInstanceOf[OutputLayer], "Output layer must be of type OutputLayer.")

  // need to define a total cost, output + regularization?
}

object MultiLayerPerceptronPlayground extends App {
  println("Hello MultiLayer Perceptron!")
}
