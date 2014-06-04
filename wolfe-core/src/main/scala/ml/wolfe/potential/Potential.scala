package ml.wolfe.potential

import ml.wolfe.FactorGraph._
import ml.wolfe.FactorieVector


/**
 * @author Sebastian Riedel
 */
trait Potential {

  def maxMarginalF2N(edge:Edge)
  def maxMarginalExpectationsAndObjective(dstExpectations:FactorieVector):Double
  def valueForCurrentSetting():Double
  def isLinear = false
  def stats():FactorieVector = null
  def toVerboseString(implicit fgPrinter: FGPrinter):String

}

final class AndPotential(arg1:Edge,arg2:Edge) extends Potential {
  def maxMarginalF2N(edge: Edge) = {
    val other = if (edge == arg1) arg2 else arg1
    edge.f2n(1) = other.n2f(1)
    edge.f2n(0) = Double.NegativeInfinity
  }
  def valueForCurrentSetting() = {
    if (arg1.n.setting == 1 && arg2.n.setting == 1) 0.0 else Double.NegativeInfinity
  }
  def maxMarginalExpectationsAndObjective(dstExpectations: FactorieVector) = {
    val beliefInConsistentSolution = arg1.n2f(1) + arg2.n2f(1)
    if (beliefInConsistentSolution > Double.NegativeInfinity) 0.0 else Double.NegativeInfinity
  }
  def toVerboseString(implicit fgPrinter: FGPrinter) = s"And(${arg1.n.index},${arg2.n.index})"

}





