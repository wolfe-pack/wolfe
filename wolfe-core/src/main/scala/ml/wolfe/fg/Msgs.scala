package ml.wolfe.fg

import ml.wolfe.FactorieVector
import ml.wolfe.MoreArrayOps._


/**
 * @author Sebastian Riedel
 */
trait Msgs {
  def asDiscrete = this.asInstanceOf[DiscreteMsgs]
  def asVector = this.asInstanceOf[VectorMsgs]

  def saveCurrentF2NAsOld()

}

final class DiscreteMsgs(val dim: Int) extends Msgs {
  val n2f     = Array.ofDim[Double](dim)
  val f2n     = Array.ofDim[Double](dim)
  val f2nLast = Array.ofDim[Double](dim)
  def saveCurrentF2NAsOld() = {
    //remember last message for calculating residuals
    set(f2n, f2nLast)
  }
}

final class VectorMsgs extends Msgs {
  var n2f    : FactorieVector = null
  var f2n    : FactorieVector = null
  var f2nLast: FactorieVector = null

  def saveCurrentF2NAsOld() = {
    f2nLast = f2n
  }
}