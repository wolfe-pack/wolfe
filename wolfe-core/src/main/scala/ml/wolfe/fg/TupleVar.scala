package ml.wolfe.fg

import ml.wolfe.FactorGraph.{Edge, Node}
import ml.wolfe.MoreArrayOps._
import ml.wolfe.util.LabelledTensor

import scala.math._
import scala.util.Random

import scalaxy.loops._

/**
 * @author luke
 */
class TupleVar(val componentNodes:Array[Node]) extends Var {
  val components = componentNodes.map(_.variable.asDiscrete)
  val dim = components.map(_.dim).product

  /* node belief */
  val b = Array.ofDim[Double](dim)
  val B = LabelledTensor.onExistingArray[DiscreteVar, Double](components, _.dim, b)

  /* external message for this node. Will usually not be updated during inference */
  val in = Array.ofDim[Double](dim)
  val IN = LabelledTensor.onExistingArray[DiscreteVar, Double](components, _.dim, in)

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
    System.arraycopy(in, 0, m.n2f.array, 0, m.n2f.array.length)
    for (e <- 0 until node.edges.length if e != edge.indexInNode) {
      m.n2f.elementWiseOp[Double](node.edges(e).msgs.asTuple.f2n, _+_)
    }
    println( "n2f: " +
      componentNodes.map(_.index.toString).mkString("(", ",", ")").padTo(10, ' ') + " -> " +
      edge.f.potential.asInstanceOf[TuplePotential].baseNodes.map(_.index.toString).mkString("(", ",", ")").padTo(10, ' ')  + " = " +
      m.n2f.array.mkString(","))
  }

  override def updateMaxMarginalBelief(node: Node) = {
    System.arraycopy(in, 0, b, 0, b.length)
    for (e <- 0 until node.edges.length) {
      B.elementWiseOp[Double](node.edges(e).msgs.asTuple.f2n, _+_)
      maxNormalize(b)
    }
    for(v <- components) {
      B.fold(Array(v), Double.NegativeInfinity, max, v.b)
    }
  }

  override def updateMarginalBelief(node: Node) = {
    /*System.arraycopy(in, 0, b, 0, b.length)
    for (e <- 0 until node.edges.length) {
      for (i <- 0 until dim)
        b(i) += node.edges(e).msgs.asDiscrete.f2n(i)
      val logZ = math.log(b.view.map(exp).sum)
      -=(b,logZ)
    }*/
    ???
  }
}
