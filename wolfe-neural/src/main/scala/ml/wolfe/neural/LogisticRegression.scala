package ml.wolfe.neural

import breeze.linalg._
import ml.wolfe.neural.math._

/**
 * Created by mbosnjak on 11/5/14.
 */

/** Logistic regression layer / classifier
 *
 * @param W internal weights
 * @param b biases
 */




class LogisticRegression(var W: DenseMatrix[Double],
                         var b: DenseVector[Double],
                         activation: (DenseMatrix[Double] => DenseMatrix[Double])) extends OutputLayer {

  //math.softmax

  // variable, as it can be changed

  var X: DenseMatrix[Double] = null

  // TODO implement generic prediction function (output function application)

  def predict(input: DenseMatrix[Double]) = math.argmax(propagateForward(input))

  def propagateForward(input: DenseMatrix[Double]): DenseMatrix[Double] = {
    X = DenseMatrix.horzcat(DenseMatrix.ones[Double](input.rows, 1), input)
    activation(X * theta)
  }

  def propagateBackward() = {

  }


  // TODO: find a nicer way to encode loss functions, errors and gradients to use them interchangeably
  // TODO: heavily test gradients
  // gradient for negative_log_likelihood
  // -(1/m) * sum(i=1:m, x(i) * ( I(y(i)=j) - p_y_given_x(i, y(i)) ) )


  // propagateBackwards
  def gradient(input: DenseMatrix[Double], y: DenseVector[Double]): DenseMatrix[Double] = {
    require(input.rows == y.length, "Number of data points and their labels is not equal!")
    val sigma = kroneckerDelta(input.rows, outputSize, y.map(_.toInt).iterator)
    val p = propagateForward(input)
    (X.t * (sigma - p)) :* (-1.0 / input.rows)
  }


  // cost
  def negative_log_likelihood(input: DenseMatrix[Double], y: DenseVector[Double]): Double = {
    val p = propagateForward(input)
    require(y.length == p.rows, "these thinglets disagree...which is awfully bad!")
    // or do this with elementwise kronecker matrix multiplications?
    var ll = 0.0
    for (i <- 0 to y.length - 1 ) {
      ll += scala.math.log(p(i, y(i).toInt))
    }
    // mean instead of sum, makes learning rate less dependent on minibatch size
    - ll / y.length.toDouble
  }


  // get errors from prediction
  def errors(input: DenseMatrix[Double], y: DenseVector[Double]): Double = {
    val output = predict(input)
    require(output.length == y.length)
    val neq = output :!= y
    neq.activeSize.toDouble / neq.length
  }

}

