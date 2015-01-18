package ml.wolfe.util

import scala.util.Random

/**
 * @author rockt
 */
object Math {
  val random = new Random(0l)

  def sigmoid(theta: Double): Double = 1 / (1 + math.exp(-theta))
}
