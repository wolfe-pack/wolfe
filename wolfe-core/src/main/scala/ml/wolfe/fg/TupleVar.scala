package ml.wolfe.fg

import ml.wolfe.FactorGraph.{Edge, Node}
import ml.wolfe.MoreArrayOps._
import ml.wolfe.util.Multidimensional._

import scala.math._
import scala.util.Random

import scalaxy.loops._

/**
 * @author luke
 */
class TupleVar(val componentNodes:Array[Node]) extends Var {
  val components = componentNodes.map(_.variable.asDiscrete)
  val dim = components.map(_.dim).product

  override val label = componentNodes.map(_.variable.label).mkString("(", ",", ")")

  /* node belief */
  val B = LabelledTensor.onNewArray[DiscreteVar, Double](components, _.dim, 0)
  val b = B.array

  /* external message for this node. Will usually not be updated during inference */
  val IN = LabelledTensor.onNewArray[DiscreteVar, Double](components, _.dim, 0)
  val in = IN.array

  /* indicates that variable is in a certain state */
  val setting: Array[Int] = Array.ofDim[Int](components.size)
  def componentSetting(v:DiscreteVar) = setting(components indexOf v)

  def updateComponentSettings() = {
    for(i <- (0 until components.length).optimized) components(i).asDiscrete.setting = setting(i)
  }

  override def entropy() = { //todo: Will BP overestimate entropy because of shared variables? Does this matter?
    var result = 0.0
    for (i <- (0 until b.length).optimized) {
      result -= math.exp(b(i)) * b(i) //messages are in log space
    }
    result
  }

  override def initializeToNegInfinity() = {
    fill(b, Double.NegativeInfinity)
  }

  override def initializeRandomly(eps:Double) = {
    for (i <- b.indices) b(i) = Random.nextGaussian() * eps
  }

  override def setup() = { }

  override def updateN2F(edge: Edge) = {
    val node = edge.n
    val m = edge.msgs.asTuple
    m.n2f.copyFrom(IN)
    for (e <- 0 until node.edges.length if e != edge.indexInNode)
      m.n2f += node.edges(e).msgs.asTuple.f2n
  }

  override def updateMaxMarginalBelief(node: Node) = {
    B.copyFrom(IN)
    for (e <- 0 until node.edges.length) {
      B += node.edges(e).msgs.asTuple.f2n
    }
    maxNormalize(b)
    for(v <- components) {
      val vB = LabelledTensor.onExistingArray[DiscreteVar, Double](Array(v), _.dim, v.b)
      B.foldInto(Double.NegativeInfinity, max, vB)
    }
  }

}
