package ml.wolfe.fg

import cc.factorie.la.SingletonTensor
import ml.wolfe.FactorGraph.{Factor, FGPrinter, Edge, Node}
import ml.wolfe.{FactorGraph, FactorieVector}
import ml.wolfe.MoreArrayOps._
import ml.wolfe.util.Multidimensional._
import scalaxy.loops._
import ml.wolfe.util.Util.approxEqual

/**
 * @author Luke
 */

trait TuplePotential extends Potential {
  val baseNodes:Array[Node] // All nodes in the original factor graph that pertain to this potential
}

/**
 * A potential that maintains consistency between two nodes of the junction tree
 */
final class TupleConsistencyPotential(edge1: Edge, edge2: Edge) extends TuplePotential {
  val v1 = edge1.n.variable.asTuple
  val v2 = edge2.n.variable.asTuple
  val m1 = edge1.msgs.asTuple
  val m2 = edge2.msgs.asTuple

  val baseNodes = v1.componentNodes intersect v2.componentNodes
  val baseVariables = baseNodes.map(_.variable.asDiscrete)

  override def toString = "Consistency " + baseNodes.map(_.index.toString).mkString("(",",",")")

  override def valueForCurrentSetting() = {
    if(baseVariables.forall (v => v1.componentSetting(v) == v2.componentSetting(v))) 0.0
    else Double.NegativeInfinity
  }

  override def maxMarginalF2N(edge: Edge) = {
    val (thisMsg, thatMsg) = if (edge == edge1) (m1, m2) else (m2, m1)

    thatMsg.n2f.foldInto(Double.NegativeInfinity, math.max, thisMsg.f2n)
    maxNormalize(thisMsg.f2n.array)
  }

  override def marginalF2N(edge: Edge) = {
    val (thisMsg, thatMsg) = if (edge == edge1) (m1, m2) else (m2, m1)

    thatMsg.n2f.foldInto(0.0, (sum:Double, x:Double) => sum + math.exp(x), thisMsg.f2n)

    normalize(thisMsg.f2n.array)
    log(thisMsg.f2n.array)
  }

  override def maxMarginalExpectationsAndObjective(result: FactorieVector) = {
    val positive1:LabelledTensor[DiscreteVar, Boolean] =
      m1.n2f.fold(baseVariables, false, (pos:Boolean, x:Double) => pos || x > Double.NegativeInfinity)
    val positive2:LabelledTensor[DiscreteVar, Boolean] =
      m1.n2f.fold(baseVariables, false, (pos:Boolean, x:Double) => pos || x > Double.NegativeInfinity)

    if((0 until positive1.array.length).exists(i => positive1.array(i) && positive2.array(i)))
      0.0 else Double.NegativeInfinity
  }
  override def toHTMLString(implicit fgPrinter: FGPrinter): String = {
    "Consistency potential for:<br/>" + baseVariables.map(_.label).mkString(", ")
  }
}

/**
 * A wrapper potential, which sums over a collection of component potentials
 * @param componentPotentials The component potentials in the junction tree
 * @param edge the edge between this GroupPotential and the TupleNode it communicates with
 */
final class GroupPotential(val componentPotentials: Array[DiscretePotential], val edge:Edge, val baseNodes:Array[Node]) extends TuplePotential {
  val v = edge.n.variable.asTuple
  val m = edge.msgs.asTuple

  def componentVariables(f:Factor) = f.edges.map(_.n.variable.asDiscrete)

  override def toString = "Group " + baseNodes.map(_.index.toString).mkString("(",",",")")

  override def valueForCurrentSetting() = {
    v.updateComponentSettings()
    componentPotentials.map(_.valueForCurrentSetting()).sum
  }

  override def marginalF2N(edge: Edge) = {
    val scoretables = componentPotentials.map(p => p.scoreTable)
    m.f2n.copyFrom(scoretables.head)
    for(t <- scoretables.tail) m.f2n += t
    maxNormalize(m.f2n.array)
  }
  override def maxMarginalF2N(edge:Edge) = marginalF2N(edge)

  override def maxMarginalExpectationsAndObjective(dstExpectations: FactorieVector) = {
    val scoretables = componentPotentials.map(p => p.scoreTable)
    val scoreSums:LabelledTensor[DiscreteVar, Double] =
      LabelledTensor.onNewArray[DiscreteVar, Double](v.components, _.dim, 0.0)
    for(t <- scoretables) scoreSums += t

    val scorePairs : LabelledTensor[DiscreteVar, (Double, Double)] =
      m.n2f.elementWiseOp[Double, (Double, Double)](scoreSums, (n2f, score) => (score, score+n2f))

    var maxScore = Double.NegativeInfinity
    var maxPenalisedScore = Double.NegativeInfinity
    var maxCount:Int = 0
    for(i <- (0 until scorePairs.array.length).optimized) {
      if (approxEqual(scorePairs.array(i)._2, maxPenalisedScore)) {
        maxCount = maxCount + 1
       } else if(scorePairs.array(i)._2 > maxPenalisedScore) {
        maxPenalisedScore = scorePairs.array(i)._2
        maxScore = scorePairs.array(i)._1
        maxCount = 1
      }
    }

    val prob = 1d / maxCount
    for (p <- componentPotentials if p.isLinear) { //todo: yuck!
      val maxMarginal = scorePairs.fold[Double](p.vars, 0, {
        (acc:Double, x:(Double, Double)) =>
          if(approxEqual(x._2, maxPenalisedScore)) acc + prob else acc
      })
      p match {
        case pot: LinearPotential =>
          val R = maxMarginal.permute(pot.edges.map(_.n.variable.asDiscrete), allowSameArray = true)
          for(i <- 0 until R.array.length if R.array(i)!=0)
            dstExpectations +=(pot.statistics.vectors(i), R.array(i))
      }
    }
    maxScore
  }

  override def toHTMLString(implicit fgPrinter: FGPrinter): String = {
    "Sum of component potentials:<br/>" + componentPotentials.map(_.toHTMLString(fgPrinter)).mkString("")
  }
}