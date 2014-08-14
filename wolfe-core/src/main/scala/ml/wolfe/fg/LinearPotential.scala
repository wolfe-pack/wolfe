package ml.wolfe.fg

import ml.wolfe.{SingletonVector, FactorGraph, FactorieVector}
import ml.wolfe.FactorGraph.{FGPrinter, Edge}
import ml.wolfe.MoreArrayOps._
import scalaxy.loops._
import cc.factorie.la.{SingletonTensor, SingletonTensor1, DenseTensor1, SparseTensor1}
import ml.wolfe.util.Util.approxEqual

/**
 * @author Sebastian Riedel
 */
final class LinearPotential(val edges: Array[Edge], val statistics: Stats, fg: FactorGraph) extends DiscretePotential {

  import statistics._

  val dims       = edges.map(_.n.variable.asDiscrete.dim)
  val entryCount = statistics.settings.size

  lazy val msgs = edges.map(_.msgs.asDiscrete)

  override def maxMarginalF2N(edge: Edge) = {
    //max over all settings
    val msgs = edge.msgs.asDiscrete
    fill(msgs.f2n, Double.NegativeInfinity)

    for (i <- 0 until settings.size) {
      val setting = settings(i)
      var score = scoreEntry(i)
      val varValue = setting(edge.indexInFactor)
      for (j <- 0 until edges.size; if j != edge.indexInFactor) {
        score += edges(j).msgs.asDiscrete.n2f(setting(j))
      }
      msgs.f2n(varValue) = math.max(score, msgs.f2n(varValue))
    }
    maxNormalize(msgs.f2n)
  }


  override def marginalF2N(edge: Edge) = {
    val msgs = edge.msgs.asDiscrete
    fill(msgs.f2n, 0.0)

    for (i <- 0 until settings.size) {
      val setting = settings(i)
      var score = scoreEntry(i)
      val varValue = setting(edge.indexInFactor)
      for (j <- 0 until edges.size; if j != edge.indexInFactor) {
        score += edges(j).msgs.asDiscrete.n2f(setting(j))
      }
      msgs.f2n(varValue) = msgs.f2n(varValue) + math.exp(score)
    }
    normalize(msgs.f2n)
    log(msgs.f2n)
  }

  override def mapF2N() = {
    for (j <- (0 until edges.size).optimized)
      fill(msgs(j).f2n, 0)

    var maxScore = Double.NegativeInfinity
    var maxSetting = Array.ofDim[Int](edges.size)

    for (i <- (0 until settings.size).optimized) {
      val setting = settings(i)
      var score = scoreEntry(i)
      for (j <- (0 until edges.size).optimized)
        score += msgs(j).n2f(setting(j))

      if(score > maxScore) {
        maxScore = score
        maxSetting = setting
      }
    }

    for (j <- (0 until edges.size).optimized)
      msgs(j).f2n(maxSetting(j)) = 1
  }

  override def valueForSetting(setting:Seq[Int]): Double = {
    val entry = TablePotential.settingToEntry(setting, dims)
    scoreEntry(entry)
  }

  override def statsForCurrentSetting() = {
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
      score += edges(j).msgs.asDiscrete.n2f(setting(j))
    }
    score
  }


  override def maxMarginalExpectationsAndObjective(dstExpectations: FactorieVector) = {
    var norm = Double.NegativeInfinity
    var maxScore = Double.NegativeInfinity
    var maxCount = 0
    //find maximum with respect to incoming messages
    //and count
    for (i <- (0 until entryCount).optimized) {
      val setting = settings(i)
      val score = penalizedScore(i, setting)
      if (approxEqual(score, norm)) {
        maxCount += 1
      }
      else if (score > norm) {
        norm = score
        maxScore = scoreEntry(i)
        maxCount = 1
      }
    }

    // prob = 1/|maxs| for all maximums, add corresponding vector
    val prob = 1.0 / maxCount
    for (i <- 0 until entryCount) {
      val setting = settings(i)
      val score = penalizedScore(i, setting)
      if (approxEqual(score, norm)) {
        dstExpectations +=(vectors(i), prob)
      }
    }
    maxScore
  }

  override def marginalExpectationsAndObjective(dstExpectations: FactorieVector) = {
    var localZ = 0.0
    //calculate local partition function
    for (i <- (0 until entryCount).optimized) {
      val setting = settings(i)
      val score = penalizedScore(i, setting)
      localZ += math.exp(score)
    }
    val logZ = math.log(localZ)
    var linear = 0.0
    var entropy = 0.0
    // prob = 1/|maxs| for all maximums, add corresponding vector
    for (i <- 0 until entryCount) {
      val setting = settings(i)
      val score = penalizedScore(i, setting)
      val prob = math.exp(score - logZ)
      linear += prob * scoreEntry(i)
      entropy -= prob * math.log(prob)
      dstExpectations +=(vectors(i), prob)
    }
    linear + entropy
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

  override def toString = "Linear " + edges.map(_.n.index.toString).mkString("(",",",")")
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