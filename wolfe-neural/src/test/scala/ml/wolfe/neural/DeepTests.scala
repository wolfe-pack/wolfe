package ml.wolfe.neural

import ml.wolfe.WolfeSpec
import breeze.linalg.{DenseVector, DenseMatrix}

/**
 * Created by mbosnjak on 11/6/14.
 */
class DeepTests extends WolfeSpec {
  "Logistic regression" should {

    "Test case 1" in {
      val x = DenseMatrix((2.0, 3.0), (2.0, 3.0), (4.0, 4.0), (5.0, -1.0), (4.0, 3.0))
      val y = DenseVector(0.0, 1.0, 0.0, 2.0, 1.0)
      val y1 = DenseVector(0.0, 1.0, 0.0, 2.0, 1.0)
      val w = DenseMatrix((2.0, 2.0, 2.0), (6.0, 4.0, 3.0))
      val b = DenseVector(1.0,2.0,3.0)
      val lr = new LogisticRegression(x, w, b)
      val learningRate = 0.02
      println("errors: " + lr.errors(y1))
      println("nll: " + lr.negative_log_likelihood(y1))
      for (i <- 0 to 100) {
        lr.theta = (lr.theta - (lr.grad(y1) :* learningRate))
        println("nll: " + lr.negative_log_likelihood(y1))
        println("errors: " + lr.errors(y1))
      }

      1 should be (1)
    }

  }
}

