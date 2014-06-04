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
  /**
   * More verbose string representation that shows that potential table depending on factor type.
   * @param fgPrinter a printer that can print nodes and factors.
   * @return A verbose string representation of this factor.
   */
  def toVerboseString(implicit fgPrinter: FGPrinter):String

}






