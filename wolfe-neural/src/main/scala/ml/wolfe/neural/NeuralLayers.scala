package ml.wolfe.neural

import breeze.linalg.{DenseVector, DenseMatrix}

/**
 * Created by mbosnjak on 11/6/14.
 */

trait NetworkLayer {
  // input is a must, determines inputSize
  val input: DenseMatrix[Double]
  val inputSize: Int = input.cols

  // number of examples determined by number of input rows
  val N: Int = input.rows

  // connection weights
  var W: DenseMatrix[Double]

  // vector of biases
  var b: DenseVector[Double]

  // this in turn can be used to produce the output of the layer
  def output: DenseMatrix[Double]
  //activation function? (in case of log reg...it is identity
}

trait OutputLayer extends NetworkLayer {
  def errors: Double
  // has error, loss, cost function
}


/*
 Hidden Layer
 Input:
 - input of the layer
 - size of the input (length of the input vector)
 - size of the output
 - W, if non-zero
 - b, if non-zero
 - activation function - tanh by default?
 Output:
 - output - the output
 */


class HiddenLayer(val input: DenseMatrix[Double],
                  hidden_size: Int = 10,
                  var W: DenseMatrix[Double] = null,
                  var b: DenseVector[Double] = null,
                  activation: (Double => Double) = scala.math.tanh) extends NetworkLayer {

//  def this(input: DenseVector[Double],
//           hidden_size: Int,
//           W: DenseMatrix[Double],
//           b: DenseVector[Double],
//           activation: (Double => Double)) = this(input.t, hidden_size, W, b, activation)


//  def _activation(q: DenseVector[Double]): DenseVector[Double] =
//    q.map(activation)
//
//  def _activation(M: DenseMatrix[Double]): DenseMatrix[Double] = {
//    for (i <- 0 to M.rows-1)
//      M(i, ::) := _activation(M(i, ::).toDenseVector).t
//    M
//  }


  // copying biases to a full matrix
//  var B = b.t
//  for (i <- 1 to B.rows-1)
//    B = DenseMatrix.vertcat(B, b.t)

  // output
//  def output = _activation(input * W + B)
  def output = input * W


  // initialize W to random values for tanh


}