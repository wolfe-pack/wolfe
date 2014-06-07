package ml.wolfe.fg

import ml.wolfe.FactorGraph._
import ml.wolfe.FactorieVector


/**
 * @author Sebastian Riedel
 */
trait Potential {

  def maxMarginalF2N(edge: Edge)
  def maxMarginalExpectationsAndObjective(dstExpectations: FactorieVector): Double
  def valueForCurrentSetting(): Double
  def gradientForAllEdges() {}
  def isLinear = false
  def stats(): FactorieVector = null
  def toVerboseString(implicit fgPrinter: FGPrinter): String

}

final class AndPotential(arg1: Edge, arg2: Edge) extends Potential {
  val n1 = arg1.n.variable.asDiscrete
  val n2 = arg2.n.variable.asDiscrete
  def maxMarginalF2N(edge: Edge) = {
    val other = if (edge == arg1) arg2 else arg1
    edge.f2n(1) = other.n2f(1)
    edge.f2n(0) = Double.NegativeInfinity
  }
  def valueForCurrentSetting() = {
    if (n1.setting == 1 && n2.setting == 1) 0.0 else Double.NegativeInfinity
  }
  def maxMarginalExpectationsAndObjective(dstExpectations: FactorieVector) = {
    val beliefInConsistentSolution = arg1.n2f(1) + arg2.n2f(1)
    if (beliefInConsistentSolution > Double.NegativeInfinity) 0.0 else Double.NegativeInfinity
  }
  def toVerboseString(implicit fgPrinter: FGPrinter) = s"And(${ arg1.n.index },${ arg2.n.index })"

}

final class ExactlyOncePotential(args: Seq[Edge]) extends Potential {
  def toVerboseString(implicit fgPrinter: FGPrinter) = {
    val argIndices = args map (_.n.index) mkString ","
    s"ExactlyOne($argIndices)"
  }
  def valueForCurrentSetting():Double = {
    var count = 0
    for (arg <- args) {
      if (arg.f2n(1) == 1) count += 1
      if (count > 1) return Double.NegativeInfinity
    }
    if (count == 1) 0.0 else Double.NegativeInfinity
  }

  def maxMarginalExpectationsAndObjective(dstExpectations: FactorieVector):Double = {
    //objective usually zero unless all choices are impossible
    for (arg <- args) if (arg.n2f(1) > Double.NegativeInfinity) return 0.0
    Double.NegativeInfinity
  }

  def maxMarginalF2N(edge: Edge) = {
    var othersFalseScore = 0.0
    var winningDelta = Double.NegativeInfinity
    for (e <- args; if e != edge) {
      othersFalseScore += e.n2f(0)
      val delta = e.n2f(1) - e.n2f(0)
      if (delta > winningDelta) {
        winningDelta = delta
      }
    }
    edge.f2n(1) = othersFalseScore
    edge.f2n(0) = othersFalseScore + winningDelta

  }
}





