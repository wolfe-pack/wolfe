package ml.wolfe.util

import cc.factorie.la.{Tensor1, DenseTensor1}
import scala.util.Random

/**
 * @author rockt
 */
object Math {
  val random = new Random(1984l)

  def sigmoid(theta: Double): Double = 1 / (1 + math.exp(-theta))

  def sigmoidDeriv(theta: Double): Double = {
    val cache = sigmoid(theta)
    (1 - cache) * cache
  }

  def logDeriv(theta: Double): Double = 1.0 / theta

  def tanh(theta: Double): Double = -1 + (2 / (1 + math.exp(-2 * theta)))

  def tanhDeriv(theta: Double): Double = 1 - math.pow(tanh(theta), 2.0)

  def elementWise(vector: Tensor1, fun: Double => Double): Tensor1 = new DenseTensor1(vector map fun)

  def vectTanh(vector: Tensor1): Tensor1 = elementWise(vector, tanh)
  def vectSigm(vector: Tensor1): Tensor1 = elementWise(vector, sigmoid)
}
