package ml.wolfe.fg

import ml.wolfe.{SingletonVector, FactorGraph, FactorieVector}
import ml.wolfe.FactorGraph.{FGPrinter, Edge}
import ml.wolfe.MoreArrayOps._
import scalaxy.loops._
import cc.factorie.la.{SingletonTensor1, DenseTensor1, SparseTensor1}


/**
 * @author Sebastian Riedel
 */
final class LinearPotential(val edges: Array[Edge], statistics: Stats, fg: FactorGraph) extends Potential {

  import statistics._

  val dims       = edges.map(_.n.variable.asDiscrete.dim)
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
  def valueForCurrentSetting() = {
    val setting = edges.map(_.n.variable.asDiscrete.setting)
    val entry = TablePotential.settingToEntry(setting, dims)
    scoreEntry(entry)
  }

  override def stats() = {
    val setting = edges.map(_.n.variable.asDiscrete.setting)
    val entry = TablePotential.settingToEntry(setting, dims)
    vectors(entry)
  }


  def scoreEntry(entry: Int) = {
    vectors(entry) match {
      //todo: unclear why, but this manual dot product is faster than calling the singleton vector dot product
      //todo: which is doing the same thing.
      case singleton: SingletonVector =>
        val index = singleton.singleIndex
        val result =
          if (index >= fg.weights.size)
          //rockt: for weights not observed during training but at test time
          //sr: this doesn't work for sparse vectors where .size only provides the number of non-zero items
          //sr: fix for now by never using sparse vectors as fg.weigths
            0.0
          else
            singleton(index) * fg.weights(index)
        result
      case vector => {
        //vector dot fg.weights
        var sum = 0.0
        val maxIndex = fg.weights.dim1
        vector.foreachActiveElement((index: Int, value: Double) =>
          if (index < maxIndex) sum += value * fg.weights(index)
        )
        sum
      }
    }

    //vectors(entry) dot fg.weights
  }

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
        maxCount = 1
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
  /**
   * More verbose string representation that shows that potential table depending on factor type.
   * @param fgPrinter a printer that can print nodes and factors.
   * @return A verbose string representation of this factor.
   */
  override def toVerboseString(implicit fgPrinter: FGPrinter) = {
    val tableString =
      for ((setting, index) <- statistics.settings.zipWithIndex) yield
        f"${ setting.mkString(" ") }%5s | ${ scoreEntry(index) }%7.4f | ${ fgPrinter.vector2String(statistics.vectors(index)) }"

    tableString.mkString("\n")
  }
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

  def sparse(entries: (Int, Double)*) = {
    val result = new SparseTensor1(100)
    for ((key, value) <- entries) result +=(key, value)
    result
  }

  def singleton(key: Int, value: Double, dim: Int = 100) = {
    new SingletonTensor1(dim, key, value)
  }

  def dense(dim: Int) = {
    new DenseTensor1(dim)
  }

  def dense(dim: Int, entries: (Int, Double)*) = {
    val result = new DenseTensor1(dim)
    for ((index, value) <- entries) result(index) = value
    result
  }


}