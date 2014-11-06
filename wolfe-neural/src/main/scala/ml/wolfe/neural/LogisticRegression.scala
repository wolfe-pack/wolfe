package ml.wolfe.neural

import breeze.linalg._
import breeze.numerics._

/**
 * Created by mbosnjak on 11/5/14.
 */


/** Logistic regression layer / classifier
 *
 * @param input matrix of examples
 * @param W internal weights
 * @param b biases
 * @param input_size size of the feature space
 * @param output_size size of the output (number of classes)
 */
class LogisticRegression(val input: DenseMatrix[Double],
                         var W: DenseMatrix[Double],
                         var b: DenseVector[Double],
                         val input_size: Int,
                         val output_size: Int) {

  // FIXED
  val N = input.rows

  // initialize if empty
  if (W == null)
    W = DenseMatrix.zeros(input_size, output_size)
  if (b == null)
    b = DenseVector.zeros[Double](output_size)

  // variable, as it can be changed
  var theta = DenseMatrix.vertcat(b.toDenseMatrix, W)

  var X = DenseMatrix.horzcat(DenseMatrix.ones[Double](input.rows, 1), input)


  def softmax(M: DenseMatrix[Double]): DenseMatrix[Double] = {
    for (i <- 0 to M.rows-1) {
      val exprow = M(i,::).inner.map(math.exp(_))
      M(i,::) := exprow.t / exprow.sum
    }
    M
  }

  def argmax(M: DenseMatrix[Double]): DenseVector[Double] = {
    val ret = DenseVector.zeros[Double](M.rows)
    for (i <- 0 to M.rows-1) {
      ret(i) = M(i, ::).inner.argmax
    }
    ret
  }

  def p_y_given_x: DenseMatrix[Double] = softmax(X * theta)

  def get_p_y_given_x(M: DenseMatrix[Double]): DenseMatrix[Double] = {
    val Q = DenseMatrix.horzcat(DenseMatrix.ones[Double](M.rows, 1), M)
    softmax(Q * theta)
  }

  def y_pred = argmax(p_y_given_x)

  def output = y_pred.toDenseMatrix

  // provide a classify function for new examples
  def classify(M: DenseMatrix[Double]) =
    argmax(get_p_y_given_x(M))




  // TODO: find a nicer way to encode loss functions, errors and gradients to use them interchangeably
  // TODO: heavily test gradients
  // gradient for negative_log_likelihood
  // -(1/m) * sum(i=1:m, x(i) * ( I(y(i)=j) - p_y_given_x(i, y(i)) ) )
  def grad(y: DenseVector[Double]) = {
    // Identity-function-matrix with 1 only where row equals to an example, and column equals to the correct prediction
    // for that example

    val sigma = DenseMatrix.zeros[Double](N, output_size)
    for (i <- 0 to N-1)
      sigma(i, y(i).toInt) = 1
    (X.t * (sigma - p_y_given_x)) :* (-1.0/N)
  }


  def negative_log_likelihood(y: DenseVector[Double]): Double = {
    require(y.length == p_y_given_x.rows)
    var ll = 0.0
    for (i <- 0 to y.length - 1 ) {
      ll += math.log(p_y_given_x(i, y(i).toInt))
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



object LogisticRegressionPlayground extends App {
  val x = DenseMatrix((2.0, 3.0), (2.0, 2.0), (4.0, 4.0), (5.0, -1.0), (4.0, 3.0))
  val y = DenseVector(0.0, 0.0, 0.0, 2.0, 0.0)
  val y1 = DenseVector(1.0, 2.0, 1.0, 0.0, 2.0)
  val w = DenseMatrix((2.0, 2.0, 2.0), (6.0, 4.0, 3.0))
  val b = DenseVector(1.0,2.0,3.0)

  val lr = new LogisticRegression(x, w, b, 2, 3)

  val learningRate = 0.02

  println("errors: " + lr.errors(y1))
  println("nll: " + lr.negative_log_likelihood(y1))
  for (i <- 0 to 100) {
    lr.theta = (lr.theta - (lr.grad(y1) :* learningRate))
    println("nll: " + lr.negative_log_likelihood(y1))
    println("errors: " + lr.errors(y1))
  }


//  lr.theta = (lr.theta - learningRate * lr.grad(y1))
//
//  println(lr.negative_log_likelihood(y1))
//  lr.theta = (lr.theta - learningRate * lr.grad(y1))



  //println(lr.W - learningRate * lr.grad(y))

  //println(lr.W - learningRate * lr.grad(y1))

  //val gradW = w

  //println(lr.W - learningRate * gradW)
  //lr.b = lr.b - learningRate * gradb

  //
  //
  //  import BIDMat.{SBMat, CMat, CSMat, DMat, Dict, IDict, FMat, GMat, GIMat, GSMat, HMat, IMat, Mat, SMat, SDMat}
  //  import BIDMat.MatFunctions._
  //  import BIDMat.SciFunctions._
  //  import BIDMat.Solvers._
  //  import BIDMat.Plotting._
  //
  //
  //  val a = rand(100,100)
  //
  //  flip; val c = a*a; val ff=gflop
  //  print(ff)
  //






}
