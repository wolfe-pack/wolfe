package ml.wolfe.fg

import ml.wolfe.FactorGraph
import ml.wolfe.FactorGraph.Edge
import scalaxy.loops._
import ml.wolfe.MoreArrayOps._


/**
 * @author Sebastian Riedel
 */
trait Msgs {
  def asDiscrete = this.asInstanceOf[DiscreteMsgs]
  def saveCurrentFN2AsOld()

}

final class DiscreteMsgs(val dim:Int) extends Msgs {
  val n2f     = Array.ofDim[Double](dim)
  val f2n     = Array.ofDim[Double](dim)
  val f2nLast = Array.ofDim[Double](dim)
  def saveCurrentFN2AsOld() = {
    //remember last message for calculating residuals
    set(f2n, f2nLast)
  }
}