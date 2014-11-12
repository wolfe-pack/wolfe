package ml.wolfe.fg20

import java.util

import ml.wolfe.MoreArrayOps._
import ml.wolfe._

import scalaxy.loops._


/**
 * @author Sebastian Riedel
 */
object TablePotential {

  def table(dims: Array[Int], pot: Array[Int] => Double) = {
    val count = dims.product
    val settings = Array.ofDim[Array[Int]](count)
    val scores = Array.ofDim[Double](count)
    for (i <- (0 until count).optimized) {
      val setting = TablePotential.entryToSetting(i, dims)
      val score = pot(setting)
      settings(i) = setting
      scores(i) = score
    }
    Table(settings, scores)
  }

  def apply(vars: Array[DiscVar[Any]], pot: Array[Int] => Double) = {
    val dims = vars.map(_.dom.size)
    new TablePotential(vars, table(dims, pot))
  }

  def apply(vars: Array[DiscVar[Any]], table: Table) = {
    new TablePotential(vars, table)
  }


  //todo: move these both into Util, to share code with LabelledTensor
  /**
   * Turns a setting vector into an entry number.
   * @param setting setting
   * @param dims dimensions of each variable.
   * @return the entry corresponding to the given setting.
   */
  final def settingToEntry(setting: Seq[Int], dims: Array[Int]) = {
    var result = 0
    for (i <- (0 until dims.length).optimized) {
      result = setting(i) + result * dims(i)
    }
    result
  }

  def allSettings(target: Array[Int], dims: Array[Int], observations: Array[Int])(body: => Unit): Unit = {
    val length = target.length
    util.Arrays.fill(target,0)
    for (i <- 0 until length) if (observations(i) != -1) target(i) = observations(i)
    var index = length - 1
    while (index >= 0) {
      //call body on current setting
      body
      //go back to the first element that hasn't yet reached its dimension, and reset everything until then
      while (index >= 0 && target(index) == dims(index) - 1) {
        if (observations(index) == -1) {
          target(index) = 0
        }
        index -= 1
      }
      //increase setting by one if we haven't yet terminated
      if (index >= 0) {
        target(index) += 1
        if (index < length - 1) index = length - 1
      }
    }
  }

  /**
   * Turns an entry into a setting
   * @param entry the entry number.
   * @param dims dimensions of the variables.
   * @return a setting array corresponding to the entry.
   */
  final def entryToSetting(entry: Int, dims: Array[Int]) = {
    val result = Array.ofDim[Int](dims.length)
    var current = entry
    for (i <- (0 until dims.length).optimized) {
      val value = current % dims(dims.length - i - 1)
      result(dims.length - i - 1) = value
      current = current / dims(dims.length - i - 1)
    }
    result
  }

  def main(args: Array[String]) {
    val target = Array.ofDim[Int](3)
    val dims = Array(2,3,3)
    val obs = Array(-1,-1,-1)
    allSettings(target,dims,obs) {
      println(target.mkString(" "))
    }
  }


}

case class Table(settings: Array[Array[Int]], scores: Array[Double])

final class TablePotential(val discVars: Array[DiscVar[Any]], table: Table) extends BruteForce.Potential
                                                                                    with MaxProduct.Potential
                                                                                    with SumProduct.Potential {

  import table._

  def settings = table.settings // in AD3GenericPotential

  val entryCount = table.scores.size
  val dims       = discVars.map(_.dom.size)

  def score(factor: BruteForce#Factor, weights: FactorieVector) = {
    val entry = TablePotential.settingToEntry(factor.discEdges.view.map(_.node.content.setting), dims)
    scores(entry)
  }


  def discMaxMarginalF2N(edge: BeliefPropagationFG#DiscEdge, weights: FactorieVector) = {
    val m = edge.msgs
    fill(m.f2n, Double.NegativeInfinity)

    for (i <- 0 until settings.length) {
      val setting = settings(i)
      var score = scores(i)
      val varValue = setting(edge.index)
      for (j <- 0 until discVars.size; if j != edge.index) {
        score += edge.factor.discEdges(j).msgs.n2f(setting(j))
      }
      m.f2n(varValue) = math.max(score, m.f2n(varValue))
    }
    maxNormalize(m.f2n)
  }


  def discMarginalF2N(edge: BeliefPropagationFG#DiscEdge, weights: FactorieVector) = {
    val m = edge.msgs
    fill(m.f2n, 0.0)

    for (i <- 0 until settings.length) {
      val setting = settings(i)
      var score = scores(i)
      val varValue = setting(edge.index)
      for (j <- 0 until discVars.length; if j != edge.index) {
        score += edge.factor.discEdges(j).msgs.n2f(setting(j))
      }
      m.f2n(varValue) = m.f2n(varValue) + math.exp(score)
    }
    //normalize
    normalize(m.f2n)
    //convert to log space
    log(m.f2n)
  }


  def penalizedScore(factor: BeliefPropagationFG#Factor, settingId: Int, setting: Array[Int]): Double = {
    var score = scores(settingId)
    for (j <- 0 until factor.discEdges.length) {
      score += factor.discEdges(j).msgs.n2f(setting(j))
    }
    score
  }


  def statsForCurrentSetting(factor: BruteForce#Factor) = isNotLinear


  def maxMarginalExpectationsAndObjective(factor: BeliefPropagationFG#Factor,
                                          dstExpectations: FactorieVector,
                                          weights: FactorieVector) = {
    // 1) go over all states, find max with respect to incoming messages
    var norm = Double.NegativeInfinity
    var maxScore = Double.NegativeInfinity
    for (i <- (0 until entryCount).optimized) {
      val setting = settings(i)
      val score = penalizedScore(factor, i, setting)
      if (score > norm) {
        norm = score
        maxScore = scores(i)
      }
    }
    maxScore
  }

  override def marginalExpectationsAndObjective(factor: BeliefPropagationFG#Factor,
                                                dstExpectations: FactorieVector,
                                                weights: FactorieVector) = {
    var localZ = 0.0

    //calculate local partition function
    for (i <- (0 until entryCount).optimized) {
      val setting = settings(i)
      val score = penalizedScore(factor, i, setting)
      localZ += math.exp(score)
    }

    var linear = 0.0
    var entropy = 0.0
    //calculate linear contribution to objective and entropy
    for (i <- (0 until entryCount).optimized) {
      val setting = settings(i)
      val score = penalizedScore(factor, i, setting)
      val prob = math.exp(score) / localZ
      linear += scores(i) * prob
      entropy -= math.log(prob) * prob
    }
    val obj = linear + entropy
    obj
  }

  def isLinear = false
}