package ml.wolfe.fg

import ml.wolfe.FactorGraph
import ml.wolfe.FactorGraph.Edge
import scalaxy.loops._


/**
 * @author Sebastian Riedel
 */
trait Msgs {
  def asDiscrete = this.asInstanceOf[DiscreteMsgs]

}

final class DiscreteMsgs(dim:Int) extends Msgs {
  val n2f     = Array.ofDim[Double](dim)
  val f2n     = Array.ofDim[Double](dim)
  val f2nLast = Array.ofDim[Double](dim)

}