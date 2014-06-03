package ml.wolfe.potential

import ml.wolfe.FactorGraph._
import ml.wolfe.FactorieVector


/**
 * @author Sebastian Riedel
 */
trait Potential {

  def maxMarginalF2N(edge:Edge)
  def maxMarginalExpectationsAndObjective(dstExpectations:FactorieVector):Double
  def value():Double
  def isLinear = false
  def stats():FactorieVector = null

}






