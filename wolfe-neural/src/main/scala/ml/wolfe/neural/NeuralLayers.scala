package ml.wolfe.neural

import breeze.linalg.{DenseVector, DenseMatrix}

/**
 * Created by mbosnjak on 11/6/14.
 */

// TODO: add activation function?
trait NetworkLayer {
  require(W.rows == input.cols, "W and input are not of adequate dimensions")
  require(b.length == W.cols, "b and W don't convey the same number of features")

  // input, connection weights, biases
  val input: DenseMatrix[Double]
  var W: DenseMatrix[Double]
  var b: DenseVector[Double]

  val inputSize: Int = input.cols
  val outputSize: Int = W.cols
  val N: Int = input.rows


  // concat b and W into theta, concat 1 and input into X
  var theta: DenseMatrix[Double]
  var X: DenseMatrix[Double]
  // needs to produce an output
  def output: DenseMatrix[Double]
}

// TODO loss and cost functions?
trait OutputLayer extends NetworkLayer {
  def errors(y: DenseVector[Double]): Double
}

/**
 *
 * @param input input examples (multiple, per-row, examples)
 * @param hiddenSize size of the hidden layer (can be inferred?)
 * @param W connection matrix
 * @param b bias vector
 * @param activation activation function
 */
class HiddenLayer(val input: DenseMatrix[Double],
                  val hiddenSize: Int,
                  var W: DenseMatrix[Double],
                  var b: DenseVector[Double],
                  activation: (Double => Double)) extends NetworkLayer {

  require(hiddenSize == W.cols, "W doesn't agree with hiddenSize")

  var theta = DenseMatrix.vertcat(b.toDenseMatrix, W)

  var X = DenseMatrix.horzcat(DenseMatrix.ones[Double](N, 1), input)

  def output = (X * theta).map(activation)
}