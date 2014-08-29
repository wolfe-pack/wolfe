package ml.wolfe.fg

import ml.wolfe.FactorGraph.Edge

/**
 * @author Sebastian Riedel
 */
class TreePotential(edges:Map[(Any,Any),Edge], multinode:Boolean) extends Potential {



  /**
   * Calculate and update the MAP messages from the factor of this potential to all edges.
   */
  override def mapF2N() = {
    val in = edges(1 -> 2).msgs.asDiscrete
    val factor2nodeMsg = in.f2n
    val node2factorMsg = in.n2f

    val trueScoreFromNode = node2factorMsg(1)

  }
  override def valueForCurrentSetting() = {
    val setting = edges(1 -> 2).n.variable.asDiscrete.value
    ???
  }
}
