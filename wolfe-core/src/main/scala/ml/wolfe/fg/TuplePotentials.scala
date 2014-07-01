package ml.wolfe.fg

import ml.wolfe.FactorGraph.{Factor, FGPrinter, Edge, Node}
import ml.wolfe.{FactorGraph, FactorieVector}
import ml.wolfe.MoreArrayOps._
import ml.wolfe.util.LabelledTensor.LabelledTensor
import ml.wolfe.util.{LabelledTensor, Util}
import scalaxy.loops._

/**
 * Created by luke on 27/06/14.
 */
trait TuplePotential extends Potential {
  val baseNodes:Array[Node]
}

final class TupleConsistencyPotential(edge1: Edge, edge2: Edge) extends TuplePotential {
  val v1 = edge1.n.variable.asTuple
  val v2 = edge2.n.variable.asTuple
  val m1 = edge1.msgs.asTuple
  val m2 = edge2.msgs.asTuple

  val baseNodes = v1.componentNodes intersect v2.componentNodes
  val baseVariables = baseNodes.map(_.variable.asDiscrete)

  override def toVerboseString(implicit fgPrinter: FGPrinter) = {
    //s"TupleConsistency(Nodes ${edge1.n.index} and ${edge2.n.index} with shared variables ${baseVariables.mkString(",")})"
    "TupleConsistency\n" + "nodes: " + baseNodes.map(_.index.toString).mkString(",")
  }

  override def valueForCurrentSetting() = {
    if(baseVariables.forall (v => v1.componentSetting(v) == v2.componentSetting(v))) 0.0
    else Double.NegativeInfinity
  }

  override def maxMarginalF2N(edge: Edge) = {
    val (thisMsg, thatMsg) = if (edge == edge1) (m1, m2) else (m2, m1)

    thatMsg.n2f.fold(baseVariables, Double.NegativeInfinity, math.max, thisMsg.f2n.array)
    maxNormalize(thisMsg.f2n.array)

    println( "f2n: " +
      baseNodes.map(_.index.toString).mkString("(", ",", ")").padTo(10, ' ') + " -> " +
      edge.n.variable.asInstanceOf[TupleVar].componentNodes.map(_.index.toString).mkString("(", ",", ")").padTo(10, ' ')  + " = " +
      thisMsg.f2n.array.mkString(","))
  }

  override def marginalF2N(edge: Edge) = {
    val (thisMsg, thatMsg) = if (edge == edge1) (m1, m2) else (m2, m1)

    thatMsg.n2f.fold(baseVariables, 0.0, (sum:Double, x:Double) => sum + math.exp(x), thisMsg.f2n.array)

    normalize(thisMsg.f2n.array)
    log(thisMsg.f2n.array)
  }

  override def maxMarginalExpectationsAndObjective(result: FactorieVector) = {
    val positive1:LabelledTensor[DiscreteVar, Boolean] =
      m1.n2f.fold[Boolean](baseVariables, false, (pos:Boolean, x:Double) => pos || x > Double.NegativeInfinity)
    val positive2:LabelledTensor[DiscreteVar, Boolean] =
      m1.n2f.fold(baseVariables, false, (pos:Boolean, x:Double) => pos || x > Double.NegativeInfinity)

    if((0 until positive1.array.length).exists(i => positive1.array(i) && positive2.array(i)))
      0.0 else Double.NegativeInfinity
  }

  /*
  //todo: Probably completely wrong
  override def marginalExpectationsAndObjective(result: FactorieVector) = {
    val unnormalisedIntersectionProbabilities = intersectionSettings.map{ s =>
      def consistentSettings1 = v1.clampedSettingsIterable(indices1, s)
      val sumConsistentExp1 = consistentSettings1.map(s1 => math.exp(m1.n2f(s1))).sum

      def consistentSettings2 = v2.clampedSettingsIterable(indices2, s)
      val sumConsistentExp2 = consistentSettings2.map(s2 => math.exp(m2.n2f(s2))).sum

      sumConsistentExp1 * sumConsistentExp2
    }

    val localZ = unnormalisedIntersectionProbabilities.sum

    val linear = localZ

    val entropy = unnormalisedIntersectionProbabilities.map{ q =>
      val prob = q / localZ
      -math.log(prob) * prob
    }.sum

    val obj = linear + entropy
    obj
  }*/
}


final class GroupPotential(val components: Array[Factor], val edge:Edge, val baseNodes:Array[Node]) extends TuplePotential {
  val v = edge.n.variable.asTuple
  val m = edge.msgs.asTuple


  def componentVariables(f:Factor) = f.edges.map(_.n.variable.asDiscrete)

  override def toVerboseString(implicit fgPrinter: FGPrinter) = {
    "Group Potential\n" + "nodes: " + baseNodes.map(_.index.toString).mkString(",")
  }

  override def valueForCurrentSetting() = {
    v.updateComponentSettings()
    components.map(_.potential.valueForCurrentSetting()).sum
  }

  override def marginalF2N(edge: Edge) = {
    val scoretables = components.map(f => f.potential.getScoreTable(componentVariables(f)))
    m.f2n.fill(x => 0)
    for(t <- scoretables) m.f2n.elementWiseOp[Double](t, _+_)

    println( "f2n: " +
      baseNodes.map(_.index.toString).mkString("(", ",", ")").padTo(10, ' ') + " -> " +
      edge.n.variable.asInstanceOf[TupleVar].componentNodes.map(_.index.toString).mkString("(", ",", ")").padTo(10, ' ')  + " = " +
      m.f2n.array.mkString(","))
  }
  override def maxMarginalF2N(edge:Edge) = marginalF2N(edge)

  override def maxMarginalExpectationsAndObjective(result: FactorieVector) = {
    val scoretables = components.map(f => f.potential.getScoreTable(componentVariables(f)))
    val scoreSums:LabelledTensor[DiscreteVar, Double] =
      LabelledTensor.onNewArray(v.components, _.dim, 0.0)
    for(t <- scoretables) scoreSums.elementWiseOp[Double](t, _+_)

    val scorePairs:LabelledTensor[DiscreteVar, (Double, Double)] =
      LabelledTensor.onNewArray(v.components, _.dim, (0.0, 0.0))

    m.n2f.elementWiseOp(scoreSums, (n2f:Double, score:Double) => (score, score+n2f), scorePairs.array)

    var maxScore = Double.NegativeInfinity
    var maxPenalisedScore = Double.NegativeInfinity
    var maxIndices:Int = 0
    for(i <- (0 until scorePairs.array.length).optimized) {
      if(scorePairs.array(i)._2 > maxPenalisedScore) {

        maxPenalisedScore = scorePairs.array(i)._2
        maxScore = scorePairs.array(i)._1
        maxIndices = 1
      } else if(scorePairs.array(i)._2 == maxPenalisedScore) {
        maxIndices = maxIndices + 1
      }
    }

    val prob = 1d / maxIndices
    for (f <- components if f.potential.isLinear) { //todo: yuck!
      val resultBoost = scorePairs.fold[Double](componentVariables(f), 0, {
        (acc:Double, x:(Double, Double)) =>
          if(x._2 == maxPenalisedScore) acc + prob else acc
      })
      f.potential match {
        case p: LinearPotential => for(i <- (0 until resultBoost.array.length).optimized) {
          result += (p.getVectors(i), resultBoost.array(i))
        }
      }
    }
    maxScore
  }
}


/*
final class WrappedPotential(val origPotential: Potential, val edge:Edge, val baseNodes:Array[Node]) extends TuplePotential {
  val v = edge.n.variable.asTuple
  val m = edge.msgs.asTuple

  val baseVariables = baseNodes.map(_.variable.asDiscrete)
  override def toVerboseString(implicit fgPrinter: FGPrinter) = {
    "WrappedPotential\n" + "nodes: " + baseNodes.map(_.index.toString).mkString(",")
  }

  override def valueForCurrentSetting() = {
    v.updateComponentSettings()
    origPotential.valueForCurrentSetting()
  }

  override def marginalF2N(edge: Edge) = {
    val scoretable = origPotential.getScoreTable(baseVariables)
    System.arraycopy(scoretable.array, 0, m.f2n.array, 0, m.f2n.array.length)
  }
  override def maxMarginalF2N(edge:Edge) = marginalF2N(edge)

  override def maxMarginalExpectationsAndObjective(result: FactorieVector) = {
    val scoretable = origPotential.getScoreTable(baseVariables)
    val scorePairs:LabelledTensor[DiscreteVar, (Double, Double)] =
      LabelledTensor.onNewArray(v.components, _.dim, (0.0, 0.0))

    m.n2f.elementWiseOp(scoretable, (n2f:Double, score:Double) => (score, score+n2f), scorePairs.array)
    var maxScore = Double.NegativeInfinity
    var maxPenalisedScore = Double.NegativeInfinity
    var maxIndices:Int = 0
    for(i <- (0 until scorePairs.array.length).optimized) {
      if(scorePairs.array(i)._2 > maxPenalisedScore) {

        maxPenalisedScore = scorePairs.array(i)._2
        maxScore = scorePairs.array(i)._1
        maxIndices = 1
      } else if(scorePairs.array(i)._2 == maxPenalisedScore) {
        maxIndices = maxIndices + 1
      }
    }

    if(origPotential.isLinear) { //todo: yuck!
      val prob = 1d / maxIndices
      val resultBoost = scorePairs.fold[Double](baseVariables, 0, {
        (acc:Double, x:(Double, Double)) =>
          if(x._2 == maxPenalisedScore) acc + prob else acc
      })
      origPotential match {
        case p: LinearPotential => {
          for(i <- (0 until resultBoost.array.length).optimized) {
            result += (p.getVectors(i), resultBoost.array(i))
          }
        }
      }
    }
    maxScore
  }
 }*/