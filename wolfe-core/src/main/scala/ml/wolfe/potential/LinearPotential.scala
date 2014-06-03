package ml.wolfe.potential

import ml.wolfe.{FactorGraph, FactorieVector}
import ml.wolfe.FactorGraph.Edge
import ml.wolfe.MoreArrayOps._
import scalaxy.loops._
import cc.factorie.la.{SingletonTensor1, DenseTensor1, SparseTensor1}


/**
 * @author Sebastian Riedel
 */
final class LinearPotential(val edges: Array[Edge], statistics: Stats, fg: FactorGraph) extends Potential {

  import statistics._

  val dims       = edges.map(_.n.dim)
  val entryCount = statistics.settings.size

  def maxMarginalF2N(edge: Edge) = {
    //max over all settings
    fill(edge.f2n, Double.NegativeInfinity)

    for (i <- 0 until settings.size) {
      val setting = settings(i)
      var score = scoreEntry(i)
      val varValue = setting(edge.indexInFactor)
      for (j <- 0 until edges.size; if j != edge.indexInFactor) {
        score += edges(j).n2f(setting(j))
      }
      edge.f2n(varValue) = math.max(score, edge.f2n(varValue))
    }
    maxNormalize(edge.f2n)
  }
  def value() = {
    val setting = edges.map(_.n.setting)
    val entry = TablePotential.settingToEntry(setting, dims)
    scoreEntry(entry)
  }

  override def stats() = {
    val setting = edges.map(_.n.setting)
    val entry = TablePotential.settingToEntry(setting, dims)
    vectors(entry)
  }


  def scoreEntry(entry: Int) = vectors(entry) dot fg.weights

  def penalizedScore(settingId: Int, setting: Array[Int]): Double = {
    var score = scoreEntry(settingId)
    for (j <- 0 until edges.size) {
      score += edges(j).n2f(setting(j))
    }
    score
  }


  def maxMarginalExpectationsAndObjective(dstExpectations: FactorieVector) = {
    var norm = Double.NegativeInfinity
    var maxScore = Double.NegativeInfinity
    var maxCount = 0
    //find maximum with respect to incoming messages
    //and count
    for (i <- (0 until entryCount).optimized) {
      val setting = settings(i)
      val score = penalizedScore(i, setting)
      if (score == norm) {
        maxCount += 1
      }
      else if (score > norm) {
        norm = score
        maxScore = scoreEntry(i)
        maxCount = 0
      }
    }
    // prob = 1/|maxs| for all maximums, add corresponding vector
    for (i <- 0 until entryCount) {
      val setting = settings(i)
      val score = penalizedScore(i, setting)
      if (score == norm) {
        dstExpectations +=(vectors(i), 1.0 / maxCount)
      }
    }
    maxScore
  }
  override def isLinear = true
}

case class Stats(settings: Array[Array[Int]], vectors: Array[FactorieVector])

object LinearPotential {
  def stats(dims: Array[Int], pot: Array[Int] => FactorieVector) = {
    val count = dims.product
    val settings = Array.ofDim[Array[Int]](count)
    val vectors = Array.ofDim[FactorieVector](count)
    for (i <- (0 until count).optimized) {
      val setting = TablePotential.entryToSetting(i, dims)
      val vector = pot(setting)
      settings(i) = setting
      vectors(i) = vector
    }
    Stats(settings, vectors)
  }
  
  def sparse(entries:(Int,Double)*) = {
    val result = new SparseTensor1(100)
    for ((key,value) <- entries) result += (key,value)
    result
  }

  def singleton(key:Int,value:Double,dim:Int=100) = {
    new SingletonTensor1(dim,key,value)
  }

  def dense(dim:Int) = {
    new DenseTensor1(dim)
  }

  def dense(dim:Int,entries:(Int,Double)*) = {
    val result = new DenseTensor1(dim)
    for ((index,value) <- entries) result(index) = value
    result
  }



}