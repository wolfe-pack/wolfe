package ml.wolfe.util

import scala.util.Random

/**
 * @author rockt
 */
object Math {
  val random = new Random(0l)

  def sigmoid(theta: Double): Double = 1 / (1 + math.exp(-theta))

  def sigmoidDeriv(theta: Double): Double = {
    val cache = sigmoid(theta)
    (1 - cache) * cache
  }

  def logDeriv(theta: Double): Double = 1.0 / theta

  def tanh(theta: Double): Double = -1 + (2 / (1 + math.exp(-2 * theta)))

  def tanhDeriv(theta: Double): Double = 1 - math.pow(tanh(theta), 2.0)
}
