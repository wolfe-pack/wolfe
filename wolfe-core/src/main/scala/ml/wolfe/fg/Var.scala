package ml.wolfe.fg

import ml.wolfe.MoreArrayOps._
import ml.wolfe.FactorGraph
import ml.wolfe.FactorGraph.{Node, Edge}
import scalaxy.loops._


/**
 * @author Sebastian Riedel
 */
trait Var {

  def asDiscrete = this.asInstanceOf[DiscreteVar]
  def setup()
  def initialize()
  def updateN2F(edge: FactorGraph.Edge)
  def updateBelief(node: FactorGraph.Node)

}

final class DiscreteVar(var dim: Int) extends Var {
  /* node belief */
  var b = Array.ofDim[Double](dim)

  /* external message for this node. Will usually not be updated during inference */
  var in = Array.ofDim[Double](dim)

  /* the domain of values. By default this corresponds to [0,dim) but can be a subset if observations are given */
  var domain: Array[Int] = _

  /* indicates that variable is in a certain state */
  var setting: Int = 0

  /* indicates the value corresponding to the setting of the node */
  var value: Int = 0


  def initialize() = {
    fill(b, Double.NegativeInfinity)
  }
  def setup() {
    if (domain == null || domain.length != dim) domain = Array.range(0, dim)
    if (b == null || b.length != dim) b = Array.ofDim[Double](dim)
    if (in == null || in.length != dim) in = Array.ofDim[Double](dim)
  }

  def updateN2F(edge: Edge) = {
    val node = edge.n
    val m = edge.msgs.asDiscrete
    System.arraycopy(in, 0, m.n2f, 0, m.n2f.length)
    for (i <- (0 until dim).optimized) {
      for (e <- (0 until node.edges.length).optimized; if e != edge.indexInNode)
        m.n2f(i) += node.edges(e).msgs.asDiscrete.f2n(i)
    }

  }
  def updateBelief(node: Node) = {
    System.arraycopy(in, 0, b, 0, b.length)
    for (e <- 0 until node.edges.length) {
      for (i <- 0 until dim)
        b(i) += node.edges(e).msgs.asDiscrete.f2n(i)
      maxNormalize(b)
    }
  }


}



