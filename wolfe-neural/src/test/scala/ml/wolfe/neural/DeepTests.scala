package ml.wolfe.neural

import ml.wolfe.WolfeSpec
import breeze.linalg.{DenseVector, DenseMatrix}

/**
 * Created by mbosnjak on 11/6/14.
 */
class DeepTests extends WolfeSpec {
  "Logistic regression" should {

    "XOR problem" in {
      val input = DenseMatrix((0.0, 1.0))

      val W = DenseMatrix((4.83, -4.63), (-4.83, 4.6))
      val b = DenseVector(-2.82, -2.74)

      val lr = new LogisticRegression(W, b)
      val o = lr.propagateForward(input)

      o(0,0) should be 7.410155028945898E-5
      o(1,0) should be 0.9999258984497106
    }
  }

  "Multilayer perceptron" should {
    // example from http://www2.econ.iastate.edu/tesfatsi/NeuralNetworks.CheungCannonNotes.pdf
    "XOR problem, forward case" in {
      val input = DenseMatrix((0.0, 1.0))

      val W = DenseMatrix((4.83, -4.63), (-4.83, 4.6))
      val b = DenseVector(-2.82, -2.74)

      val l1 = new HiddenLayer(W, b, math.ActivationFunctions.sigmoid)
      val o1 = l1.propagateForward(input)

      (o1(0,0) - 0.0005) should be < 1E-4
      (o1(0,1) - 0.8653) should be < 1E-4

      val W2 = DenseMatrix((5.73), (5.83))
      val b2 = DenseVector(-2.86)

      val l2 = new HiddenLayer(W2, b2, math.ActivationFunctions.sigmoid)
      val o2 = l2.propagateForward(o1)

      (o2(0,0) - 0.8991) should be < 1E-4
    }
  }

}

