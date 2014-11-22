package ml.wolfe.fg20

import cc.factorie.la.{DenseTensor1, SingletonTensor1, SparseTensor1}
import ml.wolfe.MoreArrayOps._
import ml.wolfe.util.Util.approxEqual
import ml.wolfe.{FactorieVector, SingletonVector}


case class Stats(settings: Array[Array[Int]], vectors: Array[FactorieVector])

object LinearPotential {
  def stats(dims: Array[Int], pot: Array[Int] => FactorieVector) = {
    val obs = Array.fill[Boolean](dims.size)(false)
    val target = Array.ofDim[Int](dims.size)
    val result = Array.ofDim[FactorieVector](dims.product)
    TablePotential.allSettings(dims, obs)(target) { i =>
      result(i) = pot(target)
    }
    result
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

/**
 * @author Sebastian Riedel
 */
final class LinearPotential(val discVars: Array[DiscVar[Any]],
                            val statistics: Array[FactorieVector]) extends MaxProduct.Potential
                                                                           with SumProduct.Potential {

  val dims = discVars.map(_.dom.size)


  def discMaxMarginalF2N(edge: BeliefPropagationFG#DiscEdge, weights: FactorieVector) = {
    //max over all settings
    val msgs = edge.msgs
    val factor = edge.factor
    fill(msgs.f2n, Double.NegativeInfinity)

    //val projectedSettings = project(settings,edge.factor.observations)

    factor.iterateDiscSettings { i =>
      var score = scoreEntry(i, weights)
      val varValue = factor.discSetting(edge.index)
      for (j <- 0 until discVars.length; if j != edge.index) {
        score += edge.factor.discEdges(j).msgs.n2f(factor.discSetting(j))
      }
      msgs.f2n(varValue) = math.max(score, msgs.f2n(varValue))
    }
    maxNormalize(msgs.f2n)
  }


  def discMarginalF2N(edge: BeliefPropagationFG#DiscEdge, weights: FactorieVector) = {

    val msgs = edge.msgs
    val factor = edge.factor

    fill(msgs.f2n, 0.0)

    factor.iterateDiscSettings { i =>
      var score = scoreEntry(i, weights)
      val varValue = factor.discSetting(edge.index)
      for (j <- 0 until discVars.length; if j != edge.index) {
        score += edge.factor.discEdges(j).msgs.n2f(factor.discSetting(j))
      }
      msgs.f2n(varValue) = msgs.f2n(varValue) + math.exp(score)
    }
    normalize(msgs.f2n)
    log(msgs.f2n) //todo: can this be done by logNormalize?
  }


  def score(factor: FG#Factor, weights: FactorieVector) = {
    val entry = TablePotential.settingToEntry(factor.discEdges.iterator.map(_.node.setting), dims)
    scoreEntry(entry, weights)
  }


  def statsForCurrentSetting(factor: FG#Factor) = {
    val entry = TablePotential.settingToEntry(factor.discEdges.iterator.map(_.node.setting), dims)
    statistics(entry)
  }


  def scoreEntry(entry: Int, weights: FactorieVector) = {
    statistics(entry) match {
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


  def maxMarginalExpectationsAndObjective(factor: BeliefPropagationFG#FactorType,
                                          dstExpectations: FactorieVector,
                                          weights: FactorieVector) = {
    var norm = Double.NegativeInfinity
    var maxScore = Double.NegativeInfinity
    var maxCount = 0
    //find maximum with respect to incoming messages
    //and count
    factor.iterateDiscSettings { i =>
      val score = penalizedScore(factor, i, factor.discSetting, weights)
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
    factor.iterateDiscSettings { i =>
      val score = penalizedScore(factor, i, factor.discSetting, weights)
      if (approxEqual(score, norm)) {
        dstExpectations +=(statistics(i), prob)
      }
    }
    maxScore
  }


  def marginalExpectationsAndObjective(factor: BeliefPropagationFG#FactorType,
                                       dstExpectations: FactorieVector,
                                       weights: FactorieVector) = {
    var localZ = 0.0
    //calculate local partition function
    factor.iterateDiscSettings { i =>
      val score = penalizedScore(factor, i, factor.discSetting, weights)
      localZ += math.exp(score)
    }
    val logZ = math.log(localZ)
    var linear = 0.0
    var entropy = 0.0
    // prob = 1/|maxs| for all maximums, add corresponding vector
    factor.iterateDiscSettings { i =>
      val score = penalizedScore(factor, i, factor.discSetting, weights)
      val prob = math.exp(score - logZ)
      linear += prob * scoreEntry(i, weights)
      entropy -= prob * math.log(prob)
      dstExpectations +=(statistics(i), prob)
    }
    linear + entropy
  }


  def isLinear = true

  override def toString = "Linear " + discVars.map(_.name).mkString("(", ",", ")")
}

/**
 * @author Sebastian Riedel
 */
final class LinearPotential2(val discVars: Array[DiscVar[Any]], weightsVar:VectVar,
                             val statistics: Array[FactorieVector]) extends MaxProduct.ExpFamPotential
                                                                            with SumProduct.ExpFamPotential {

  val discDims = discVars.map(_.dom.size)
  val contVars = Array.ofDim[ContVar](0)
  val vectVars = Array(weightsVar)

  def processor() = new Proc

  class Proc extends MaxProduct.ExpFamProcessor with SumProduct.ExpFamProcessor with TableBasedProcessor {

    def dims = discDims

    def scoreTableEntry(entry: Int, setting: Setting) = {
      statistics(entry) dot weights(setting)
    }

    def maxMarginalExpectationsAndObjective(partialSetting: PartialSetting,
                                            incoming: Msgs,
                                            dstExpectations: FactorieVector) = {
      var norm = Double.NegativeInfinity
      var maxScore = Double.NegativeInfinity
      var maxCount = 0
      //find maximum with respect to incoming messages
      //and count
      TablePotential.allSettings(dims, partialSetting.discObs)(partialSetting.disc){ i =>
        val score = penalizedScore(incoming,i, partialSetting)
        if (approxEqual(score, norm)) {
          maxCount += 1
        }
        else if (score > norm) {
          norm = score
          maxScore = scoreTableEntry(i,partialSetting)
          maxCount = 1
        }
      }

      // prob = 1/|maxs| for all maximums, add corresponding vector
      val prob = 1.0 / maxCount
      TablePotential.allSettings(dims, partialSetting.discObs)(partialSetting.disc){ i =>
        val score = penalizedScore(incoming, i, partialSetting)
        if (approxEqual(score, norm)) {
          dstExpectations +=(statistics(i), prob)
        }
      }
      maxScore
    }


    def marginalExpectationsAndObjective(partialSetting: PartialSetting, incoming: Msgs, dstExpectations: FactorieVector) = {
      var localZ = 0.0
      //calculate local partition function
      TablePotential.allSettings(dims, partialSetting.discObs)(partialSetting.disc){ i =>
        val score = penalizedScore(incoming,i, partialSetting)
        localZ += math.exp(score)
      }
      val logZ = math.log(localZ)
      var linear = 0.0
      var entropy = 0.0
      // prob = 1/|maxs| for all maximums, add corresponding vector
      TablePotential.allSettings(dims, partialSetting.discObs)(partialSetting.disc){ i =>
        val score = penalizedScore(incoming, i, partialSetting)
        val prob = math.exp(score - logZ)
        linear += prob * scoreTableEntry(i, partialSetting)
        entropy -= prob * math.log(prob)
        dstExpectations +=(statistics(i), prob)
      }
      linear + entropy
    }
    def stats(setting: Setting) = {
      val entry = TablePotential.settingToEntry(setting.disc, dims)
      statistics(entry)
    }

  }



  def discMaxMarginalF2N(edge: BeliefPropagationFG#DiscEdge, weights: FactorieVector) = {
    //max over all settings
    val msgs = edge.msgs
    val factor = edge.factor
    fill(msgs.f2n, Double.NegativeInfinity)

    //val projectedSettings = project(settings,edge.factor.observations)

    factor.iterateDiscSettings { i =>
      var score = scoreEntry(i, weights)
      val varValue = factor.discSetting(edge.index)
      for (j <- 0 until discVars.length; if j != edge.index) {
        score += edge.factor.discEdges(j).msgs.n2f(factor.discSetting(j))
      }
      msgs.f2n(varValue) = math.max(score, msgs.f2n(varValue))
    }
    maxNormalize(msgs.f2n)
  }


  def discMarginalF2N(edge: BeliefPropagationFG#DiscEdge, weights: FactorieVector) = {

    val msgs = edge.msgs
    val factor = edge.factor

    fill(msgs.f2n, 0.0)

    factor.iterateDiscSettings { i =>
      var score = scoreEntry(i, weights)
      val varValue = factor.discSetting(edge.index)
      for (j <- 0 until discVars.length; if j != edge.index) {
        score += edge.factor.discEdges(j).msgs.n2f(factor.discSetting(j))
      }
      msgs.f2n(varValue) = msgs.f2n(varValue) + math.exp(score)
    }
    normalize(msgs.f2n)
    log(msgs.f2n) //todo: can this be done by logNormalize?
  }


  def score(factor: FG#Factor, weights: FactorieVector) = {
    val entry = TablePotential.settingToEntry(factor.discEdges.iterator.map(_.node.setting), discDims)
    scoreEntry(entry, weights)
  }


  def statsForCurrentSetting(factor: FG#Factor) = {
    val entry = TablePotential.settingToEntry(factor.discEdges.iterator.map(_.node.setting), discDims)
    statistics(entry)
  }


  def scoreEntry(entry: Int, weights: FactorieVector) = {
    statistics(entry) match {
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
    penalizedScore(factor, i, TablePotential.entryToSetting(i, discDims), weights)

  def penalizedScore(factor: BeliefPropagationFG#Factor, settingId: Int, setting: Array[Int], weights: FactorieVector): Double = {
    var score = scoreEntry(settingId, weights)
    for (j <- 0 until factor.discEdges.length) {
      score += factor.discEdges(j).msgs.n2f(setting(j))
    }
    score
  }


  def maxMarginalExpectationsAndObjective(factor: BeliefPropagationFG#FactorType,
                                          dstExpectations: FactorieVector,
                                          weights: FactorieVector) = {
    var norm = Double.NegativeInfinity
    var maxScore = Double.NegativeInfinity
    var maxCount = 0
    //find maximum with respect to incoming messages
    //and count
    factor.iterateDiscSettings { i =>
      val score = penalizedScore(factor, i, factor.discSetting, weights)
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
    factor.iterateDiscSettings { i =>
      val score = penalizedScore(factor, i, factor.discSetting, weights)
      if (approxEqual(score, norm)) {
        dstExpectations +=(statistics(i), prob)
      }
    }
    maxScore
  }


  def marginalExpectationsAndObjective(factor: BeliefPropagationFG#FactorType,
                                       dstExpectations: FactorieVector,
                                       weights: FactorieVector) = {
    var localZ = 0.0
    //calculate local partition function
    factor.iterateDiscSettings { i =>
      val score = penalizedScore(factor, i, factor.discSetting, weights)
      localZ += math.exp(score)
    }
    val logZ = math.log(localZ)
    var linear = 0.0
    var entropy = 0.0
    // prob = 1/|maxs| for all maximums, add corresponding vector
    factor.iterateDiscSettings { i =>
      val score = penalizedScore(factor, i, factor.discSetting, weights)
      val prob = math.exp(score - logZ)
      linear += prob * scoreEntry(i, weights)
      entropy -= prob * math.log(prob)
      dstExpectations +=(statistics(i), prob)
    }
    linear + entropy
  }


  def isLinear = true

  override def toString = "Linear " + discVars.map(_.name).mkString("(", ",", ")")
}

