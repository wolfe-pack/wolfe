package ml.wolfe.neural

import breeze.linalg._
import ml.wolfe.neural.math._

/**
 * Created by mbosnjak on 11/5/14.
 */

/** Logistic regression layer / classifier
 *
 * @param input matrix of examples
 * @param W internal weights
 * @param b biases
 */
class LogisticRegression(val input: DenseMatrix[Double],
                         var W: DenseMatrix[Double],
                         var b: DenseVector[Double]) extends OutputLayer {



  // variable, as it can be changed
  var theta = DenseMatrix.vertcat(b.toDenseMatrix, W)


  var X = DenseMatrix.horzcat(DenseMatrix.ones[Double](input.rows, 1), input)


  def p_y_given_x: DenseMatrix[Double] = math.softmax(X * theta)


  def get_p_y_given_x(M: DenseMatrix[Double]): DenseMatrix[Double] = {
    val Q = DenseMatrix.horzcat(DenseMatrix.ones[Double](M.rows, 1), M)
    math.softmax(Q * theta)
  }


  def y_pred = math.argmax(p_y_given_x)


  def output = y_pred.toDenseMatrix


  // provide a classify function for new examples
  def classify(M: DenseMatrix[Double]) =
    math.argmax(get_p_y_given_x(M))


  // TODO: find a nicer way to encode loss functions, errors and gradients to use them interchangeably
  // TODO: heavily test gradients
  // gradient for negative_log_likelihood
  // -(1/m) * sum(i=1:m, x(i) * ( I(y(i)=j) - p_y_given_x(i, y(i)) ) )
  def grad(y: DenseVector[Double]) = {
    // Identity-function-matrix with 1 only where row equals to an example, and column equals to the correct prediction
    // for that example
    kroneckerDelta(N, outputSize, y.map(_.toInt).iterator)
    val sigma = DenseMatrix.zeros[Double](N, outputSize)
    for (i <- 0 to N-1)
      sigma(i, y(i).toInt) = 1
    (X.t * (sigma - p_y_given_x)) :* (-1.0 / N)
  }


  def negative_log_likelihood(y: DenseVector[Double]): Double = {
    require(y.length == p_y_given_x.rows)
    var ll = 0.0
    for (i <- 0 to y.length - 1 ) {
      ll += scala.math.log(p_y_given_x(i, y(i).toInt))
    }
    // mean instead of sum, makes learning rate less dependent on minibatch size
    - ll / y.length.toDouble //N
  }


  def errors(y: DenseVector[Double]): Double = {
    require(y_pred.length == y.length)
    val neq = y_pred :!= y
    neq.activeSize.toDouble / neq.length
  }

}

