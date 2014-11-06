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

}

object MultiLayerPerceptronPlayground extends App {



//  val input = DenseMatrix((1.0,0.0),(0.0,1.0))
//  val W = DenseMatrix((1.0,2.0),(2.0,1.0))
//  val b = DenseVector[Double](1.0,1.0)
//  val output_size = 2

  //val lr = new LogisticRegression(input, W, b, output_size)

//  val c = -b
//
//  println(b)
//  println(c)
//  println(c :!= b)






}