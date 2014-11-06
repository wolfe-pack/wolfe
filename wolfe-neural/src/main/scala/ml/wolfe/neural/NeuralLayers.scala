package ml.wolfe.neural

import breeze.linalg.{DenseVector, DenseMatrix}

/**
 * Created by mbosnjak on 11/6/14.
 */

// TODO: add activation function?
trait NetworkLayer {
  require(b.length == W.cols, "b and W don't convey the same number of features")

  // input, connection weights, biases
  var W: DenseMatrix[Double]
  var b: DenseVector[Double]

  val inputSize: Int = W.rows
  val outputSize: Int = W.cols

  //def propagateForward(input: DenseMatrix[Double]): DenseMatrix[Double]
  //def backwardPropagate = ???


  // concat b and W into theta, concat 1 and input into X
  var theta = DenseMatrix.vertcat(b.toDenseMatrix, W)
  //var X: DenseMatrix[Double]
  // needs to produce an output
  //def output: DenseMatrix[Double]
}

// TODO loss and cost functions?
trait OutputLayer extends NetworkLayer {
  def errors(input: DenseMatrix[Double], y: DenseVector[Double]): Double
}

/**
 *
 * @param W connection matrix
 * @param b bias vector
 * @param activation activation function
 */
class HiddenLayer(var W: DenseMatrix[Double],
                  var b: DenseVector[Double],
                  activation: (Double => Double)) extends NetworkLayer {

  var X: DenseMatrix[Double] = null

  def propagateForward(input: DenseMatrix[Double]): DenseMatrix[Double] = {
    X = DenseMatrix.horzcat(DenseMatrix.ones[Double](input.rows, 1), input)
    val mul: DenseMatrix[Double] = X * theta
    mul.map(activation)
  }

}