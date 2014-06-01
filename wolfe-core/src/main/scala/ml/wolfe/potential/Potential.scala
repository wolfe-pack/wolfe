package ml.wolfe.potential

import ml.wolfe.FactorGraph._
import scalaxy.loops._
import ml.wolfe.{FactorieVector, FactorGraph}
import ml.wolfe.MoreArrayOps._


/**
 * @author Sebastian Riedel
 */
trait Potential {


  def maxMarginalF2N(edge:Edge)
  def maxMarginalExpectationsAndObjective(dstExpectations:FactorieVector):Double
  def value():Double

}

case class Table(settings:Array[Array[Int]],scores:Array[Double])

final class TablePotential(edges:Array[Edge],table:Table) extends Potential {

  import table._

  val entryCount = edges.size
  val dims = edges.map(_.n.dim)

  def value() = {
    val setting = edges.map(_.n.setting)
    val entry = FactorGraph.settingToEntry(setting, dims)
    scores(entry)
  }

  def penalizedScore(settingId: Int, setting: Array[Int]): Double = {
    var score = scores(settingId)
    for (j <- 0 until edges.size) {
      score += edges(j).n2f(setting(j))
    }
    score
  }

  def maxMarginalF2N(edge: Edge) = {
    //max over all settings
    fill(edge.f2n, Double.NegativeInfinity)

    for (i <- (0 until settings.size).optimized) {
      val setting = settings(i)
      var score = scores(i)
      val varValue = setting(edge.indexInFactor)
      for (j <- (0 until edges.size).optimized; if j != edge.indexInFactor) {
        score += edges(j).n2f(setting(j))
      }
      edge.f2n(varValue) = math.max(score, edge.f2n(varValue))
    }
    maxNormalize(edge.f2n)

  }
  def maxMarginalExpectationsAndObjective(result: FactorieVector) = {
    // 1) go over all states, find max with respect to incoming messages
    var norm = Double.NegativeInfinity
    var maxScore = Double.NegativeInfinity
    for (i <- (0 until entryCount).optimized) {
      val setting = settings(i)
      val score = penalizedScore(i, setting)
      if (score > norm) {
        norm = score
        maxScore = scores(i)
      }
    }
    maxScore
  }
}

object TablePotential {

  def table(dims:Array[Int],pot:Array[Int] => Double) = {
    val count = dims.product
    val settings = Array.ofDim[Array[Int]](count)
    val scores = Array.ofDim[Double](count)
    for (i <- (0 until count).optimized) {
      val setting = FactorGraph.entryToSetting(i,dims)
      val score = pot(setting)
      settings(i) = setting
      scores(i) = score
    }
    Table(settings,scores)
  }

  def apply(edges:Array[Edge],pot:Array[Int] => Double) = {
    val dims = edges.map(_.n.dim)
    new TablePotential(edges,table(dims,pot))
  }
  def apply(edges:Array[Edge],table:Table) = {
    new TablePotential(edges,table)
  }

}

