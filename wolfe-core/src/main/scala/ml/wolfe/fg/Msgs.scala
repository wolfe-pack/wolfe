package ml.wolfe.fg

import ml.wolfe.FactorieVector
import ml.wolfe.MoreArrayOps._
import ml.wolfe.util.Multidimensional._


/**
 * @author Sebastian Riedel
 */
trait Msgs {
  def asDiscrete = this.asInstanceOf[DiscreteMsgs]
  def asVector = this.asInstanceOf[VectorMsgs]
  def asTuple = this.asInstanceOf[TupleMsgs]
  def saveCurrentF2NAsOld()

}

final class DiscreteMsgs(val dim: Int) extends Msgs {
  val n2f     = Array.ofDim[Double](dim)
  val f2n     = Array.ofDim[Double](dim)
  val f2nLast = Array.ofDim[Double](dim)
  def saveCurrentF2NAsOld() = {
    //remember last message for calculating residuals
    set(f2nLast, f2n)
  }
}

/**
 *
 * @param n2fComponents The components of the n2f message
 * @param f2nComponents The components of the f2n message
 */
final class TupleMsgs(n2fComponents:Array[DiscreteVar[_]], f2nComponents:Array[DiscreteVar[_]]) extends Msgs {
  val n2f     = LabelledTensor.onNewArray[DiscreteVar[_], Double](n2fComponents, _.dim, 0.0)
  val f2nLast = LabelledTensor.onNewArray[DiscreteVar[_], Double](f2nComponents, _.dim, 0.0)
  val f2n     = LabelledTensor.onNewArray[DiscreteVar[_], Double](f2nComponents, _.dim, 0.0)
  def saveCurrentF2NAsOld() = {
    set(f2nLast.array, f2n.array)
    //remember last message for calculating residuals
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