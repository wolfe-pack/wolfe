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
final class LinearPotential2(val discVars: Array[DiscVar[Any]], weightsVar:VectVar,
                             val statistics: Array[FactorieVector]) extends MaxProduct.ExpFamPotential
                                                                            with SumProduct.ExpFamPotential {

  val discDims = discVars.map(_.dom.size)
  val contVars = Array.ofDim[ContVar](0)
  val vectVars = Array(weightsVar)

  def processor() = new Proc


  def score(factor: FG#Factor, weights: FactorieVector) = ???
  def statsForCurrentSetting(factor: FG#Factor) = ???


  def isLinear = true
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


  override def toString = "Linear " + discVars.map(_.name).mkString("(", ",", ")")
}

