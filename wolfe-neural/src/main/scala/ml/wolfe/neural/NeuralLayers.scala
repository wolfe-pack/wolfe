package ml.wolfe.neural

import breeze.linalg.{DenseVector, DenseMatrix}

/**
 * Created by mbosnjak on 11/6/14.
 */

// TODO: add activation function?
trait NetworkLayer {
  require(b.length == W.cols, "b and W don't convey the same number of features")

  var W: DenseMatrix[Double]                              // connection weights
  var b: DenseVector[Double]                              // biases

  val inputSize: Int = W.rows
  val outputSize: Int = W.cols

  def propagateForward(input: DenseMatrix[Double]): DenseMatrix[Double]
  //def backwardPropagate = ???

  var theta = DenseMatrix.vertcat(b.toDenseMatrix, W)     // concat b and W into theta
  var X: DenseMatrix[Double]                              // concat 1 and input into X
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