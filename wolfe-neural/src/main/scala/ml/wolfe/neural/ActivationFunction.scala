package ml.wolfe.neural

import breeze.linalg.DenseMatrix

import scala.math._

/**
 * Created by narad on 11/28/14.
 */
abstract class ActivationFunction {

//  def apply(loss: LossFunction): Double

  def f: (Double => Double)

  def df: (Double => Double)

}

class SigmoidActivationFunction extends ActivationFunction {

  def sigmoid(x: Double) = 1.0 / (1.0 + exp(-x))

  def δ_sigmoid(x: Double) = sigmoid(x) * (1 - sigmoid(x))

  def f = sigmoid(_)

  def df = δ_sigmoid(_)

}

class TanhActivationFunction extends ActivationFunction {

  def tanh(x: Double) = scala.math.tanh(x)

  def δ_tanh(x: Double) = 1.0 - tanh(x) * tanh(x)

  def f = tanh(_)

  def df = δ_tanh(_)
}


abstract class NeuralLossFunction() {

  def apply(layer: NetworkLayer, y: DenseMatrix[Double]): DenseMatrix[Double]

}

class SquaredErrorLoss extends NeuralLossFunction {

  def apply(layer: NetworkLayer, y: DenseMatrix[Double] = null): DenseMatrix[Double] = {
    println("idx = " + layer.idx)
    layer match {
      case h: HiddenLayer => {
//        println("hidden")
        // val lastW: DenseMatrix[Double] = layer.next.get.W
        (layer.next.get.W * y) :* layer.theta.map(layer.activation.df)
      }
      case o: OutputLayer => {
 //       println("output")
        (y - layer.theta.map(layer.activation.f)).t :* layer.theta.map(layer.activation.df)
      }
    }
  }
}
