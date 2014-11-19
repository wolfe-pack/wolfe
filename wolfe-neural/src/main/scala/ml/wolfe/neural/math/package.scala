package ml.wolfe.neural

import breeze.linalg.{DenseVector, DenseMatrix, sum}
import scala.math._
import breeze.stats.distributions.Rand

/**
 * Created by mbosnjak on 11/6/14.
 */
package object math {

  def argmax(M: DenseMatrix[Double]): DenseVector[Double] = {
    val ret = DenseVector.zeros[Double](M.rows)
    for (i <- 0 to M.rows-1) {
      ret(i) = breeze.linalg.argmax(M(i, ::).inner)
    }
    ret
  }

  def kroneckerDelta(i: Int, j: Int): Double = if (i == j) 1.0 else 0.0

  def kroneckerDelta(rows: Int, cols: Int, iter: Iterator[(Int, Int)]): DenseMatrix[Double] = {
    val sigma = DenseMatrix.zeros[Double](rows, cols)
    for (it <- iter) {
      sigma(it._1, it._2) = 1
    }
    sigma
  }

  object RegularizationFunctions {
    def LpRegularization(v: DenseVector[Double], p: Double): Double = {
      def Lp_fun: (Double => Double) = x => scala.math.pow(scala.math.abs(x), p)
      scala.math.pow(sum(v.map(Lp_fun)), 1.0 / p)
    }
    def L1Regularization(v: DenseVector[Double]): Double = {
      scala.math.pow(sum(v.map(x => x * x)), 0.5)
    }
    def L2Regularization(v: DenseVector[Double]): Double = {
      sum(v.map(scala.math.abs))
    }

  }

  object ActivationFunctions {

    // matrix-based activation functions

    def softmax(M: DenseMatrix[Double]): DenseMatrix[Double] = {
      for (i <- 0 to M.rows-1) {
        val exprow = M(i,::).inner.map(scala.math.exp(_))
        M(i,::) := exprow.t / sum(exprow)
      }
      M
    }


    // vector-based activation functions



    def softmax(v: DenseVector[Double]) = v.map(exp(_)) / sum(v)
    def δ_softmax(v: DenseVector[Double], i: Int, j: Int) = v(i) * (kroneckerDelta(i,j) - v(j))


    // single score -based activation functions

    def sigmoid(x: Double, alpha: Double) = 1.0 / (1.0 + exp(-x * alpha))
    def δ_sigmoid(x: Double, alpha: Double) = sigmoid(x, alpha) * (1 - sigmoid(x, alpha))

    def sigmoid(x: Double) = 1.0 / (1.0 + exp(-x))
    def δ_sigmoid(x: Double) = sigmoid(x) * (1 - sigmoid(x))

    def tanh(x: Double) = scala.math.tanh(x)
    def δ_tanh(x: Double) = 1.0 - tanh(x) * tanh(x)

    def ReLU(x: Double) = max(0.0, x)
    def δ_ReLU(x: Double) = if (x > 0) 1.0 else 0.0

    def noisyReLU(x: Double, mean: Double, std: Double) = max(0.0, x + Rand.gaussian(mean, std).draw())
    def δ_noisyReLU(x: Double) = δ_ReLU(x)

    def leakyReLU(x: Double) = if (x > 0) x else 0.01 * x
    def δ_leakyReLU(x: Double) = if (x > 0) 1.0 else 0.01

    def softplus(x: Double) = log(1.0 + exp(x))
    def δ_softplus(x: Double) = {val e = exp(x); e / (1.0 + e)}

    def step(x: Double) = if (x >= 0) 1 else 0

    def sign(x: Double) = if (x >= 0) 1 else -1

    def linear(x: Double) = x
    def δ_linear(x: Double) = 1

  }

  object WeightInitialization {

    private def activationDependent(rows: Int, cols: Int, multiplier: Double): DenseMatrix[Double] = {
      val root = sqrt(6.0 / (rows + cols))
      DenseMatrix.rand(rows, cols, Rand.uniform.map(x => multiplier * (0.5 - x) * root))
    }

    def forTanh(rows: Int, cols: Int): DenseMatrix[Double] = {
      activationDependent(rows, cols, 1)
    }

    def forSigmoid(rows: Int, cols: Int): DenseMatrix[Double] = {
      activationDependent(rows, cols, 4)
    }

    def zeros(rows: Int, cols: Int): DenseMatrix[Double] =
      DenseMatrix.zeros(rows, cols)

    def uniformPlusMinusOne(rows: Int, cols: Int): DenseMatrix[Double] = {
      DenseMatrix.rand(rows, cols, Rand.uniform.map(x => (0.5 - x) * 2))
    }
  }


}
