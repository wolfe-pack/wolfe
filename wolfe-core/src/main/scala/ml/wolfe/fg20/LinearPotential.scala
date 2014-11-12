package ml.wolfe.fg20

import cc.factorie.la.{DenseTensor1, SingletonTensor1, SparseTensor1}
import ml.wolfe.MoreArrayOps._
import ml.wolfe.util.Util.approxEqual
import ml.wolfe.{FactorieVector, SingletonVector}

import scalaxy.loops._

/**
 * @author Sebastian Riedel
 */
final class LinearPotential(val discVars: Array[DiscVar[Any]],
                            val statistics: Stats) extends BruteForce.Potential
                                                           with MaxProduct.Potential
                                                           with SumProduct.Potential {

  import statistics._

  val settings   = statistics.settings
  val entryCount = settings.size
  val dims       = discVars.map(_.dom.size)


  def discMaxMarginalF2N(edge: BeliefPropagationFG#DiscEdge, weights: FactorieVector) = {
    //max over all settings
    val msgs = edge.msgs
    fill(msgs.f2n, Double.NegativeInfinity)

    //val projectedSettings = project(settings,edge.factor.observations)

    for (i <- 0 until settings.size) {
      val setting = settings(i)
      var score = scoreEntry(i, weights)
      val varValue = setting(edge.index)
      for (j <- 0 until discVars.length; if j != edge.index) {
        score += edge.factor.discEdges(j).msgs.n2f(setting(j))
      }
      msgs.f2n(varValue) = math.max(score, msgs.f2n(varValue))
    }
    maxNormalize(msgs.f2n)
  }


  def discMarginalF2N(edge: BeliefPropagationFG#DiscEdge, weights: FactorieVector) = {

    val msgs = edge.msgs
    fill(msgs.f2n, 0.0)

    for (i <- 0 until settings.length) {
      val setting = settings(i)
      var score = scoreEntry(i, weights)
      val varValue = setting(edge.index)
      for (j <- 0 until discVars.length; if j != edge.index) {
        score += edge.factor.discEdges(j).msgs.n2f(setting(j))
      }
      msgs.f2n(varValue) = msgs.f2n(varValue) + math.exp(score)
    }
    normalize(msgs.f2n)
    log(msgs.f2n) //todo: can this be done by logNormalize?
  }


  def score(factor: BruteForce#Factor, weights: FactorieVector) = {
    val entry = TablePotential.settingToEntry(factor.discSetting, dims)
    scoreEntry(entry, weights)
  }


  def statsForCurrentSetting(factor: BruteForce#Factor) = {
    val entry = TablePotential.settingToEntry(factor.discSetting, dims)
    vectors(entry)
  }


  def scoreEntry(entry: Int, weights: FactorieVector) = {
    vectors(entry) match {
      //todo: unclear why, but this manual dot product is faster than calling the singleton vector dot product
      //todo: which is doing the same thing.
      case singleton: SingletonVector =>
        val index = singleton.singleIndex
        val result =
          if (index >= weights.size)
          //rockt: for weights not observed during training but at test time
          //sr: this doesn't work for sparse vectors where .size only provides the number of non-zero items
          //sr: fix for now by never using sparse vectors as fg.weigths
            0.0
          else
            singleton(index) * weights(index)
        result
      case vector =>
        //vector dot fg.weights
        var sum = 0.0
        val maxIndex = weights.dim1
        vector.foreachActiveElement((index: Int, value: Double) =>
          if (index < maxIndex) sum += value * weights(index)
        )
        sum
    }

    //vectors(entry) dot fg.weights
  }

  def penalizedScore(factor: BeliefPropagationFG#Factor, i: Int, weights: FactorieVector): Double =
    penalizedScore(factor, i, TablePotential.entryToSetting(i, dims), weights)

  def penalizedScore(factor: BeliefPropagationFG#Factor, settingId: Int, setting: Array[Int], weights: FactorieVector): Double = {
    var score = scoreEntry(settingId, weights)
    for (j <- 0 until factor.discEdges.length) {
      score += factor.discEdges(j).msgs.n2f(setting(j))
    }
    score
  }


  def maxMarginalExpectationsAndObjective(factor: BeliefPropagationFG#Factor,
                                          dstExpectations: FactorieVector,
                                          weights: FactorieVector) = {
    var norm = Double.NegativeInfinity
    var maxScore = Double.NegativeInfinity
    var maxCount = 0
    //find maximum with respect to incoming messages
    //and count
    for (i <- (0 until entryCount).optimized) {
      val setting = settings(i)
      val score = penalizedScore(factor, i, setting, weights)
      if (approxEqual(score, norm)) {
        maxCount += 1
      }
      else if (score > norm) {
        norm = score
        maxScore = scoreEntry(i, weights)
        maxCount = 1
      }
    }

    // prob = 1/|maxs| for all maximums, add corresponding vector
    val prob = 1.0 / maxCount
    for (i <- 0 until entryCount) {
      val setting = settings(i)
      val score = penalizedScore(factor, i, setting, weights)
      if (approxEqual(score, norm)) {
        dstExpectations +=(vectors(i), prob)
      }
    }
    maxScore
  }


  def marginalExpectationsAndObjective(factor: BeliefPropagationFG#Factor,
                                       dstExpectations: FactorieVector,
                                       weights: FactorieVector) = {
    var localZ = 0.0
    //calculate local partition function
    for (i <- (0 until entryCount).optimized) {
      val setting = settings(i)
      val score = penalizedScore(factor, i, setting, weights)
      localZ += math.exp(score)
    }
    val logZ = math.log(localZ)
    var linear = 0.0
    var entropy = 0.0
    // prob = 1/|maxs| for all maximums, add corresponding vector
    for (i <- 0 until entryCount) {
      val setting = settings(i)
      val score = penalizedScore(factor, i, setting, weights)
      val prob = math.exp(score - logZ)
      linear += prob * scoreEntry(i, weights)
      entropy -= prob * math.log(prob)
      dstExpectations +=(vectors(i), prob)
    }
    linear + entropy
  }


  def isLinear = true

  override def toString = "Linear " + discVars.map(_.name).mkString("(", ",", ")")
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