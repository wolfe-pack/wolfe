package ml.wolfe.fg20

import cc.factorie.maths._
import ml.wolfe.MoreArrayOps._
import ml.wolfe.{FactorieVector, SparseVector}

import scala.util.Random

/**
 * @author Sebastian Riedel
 */
abstract class GibbsSampler(val problem: Problem)
                           (implicit random: Random = new Random(0)) extends FG with NodeContentFG with EmptyEdgeFG {

  class NodeContent
  final class DiscNodeContent(size: Int) extends NodeContent {
    val probs      = Array.ofDim[Double](size)
    val belief     = Array.ofDim[Double](size)
    var lastUpdate = -1

  }
  final class ContNodeContent() extends NodeContent {
    var mean       = 0.0
    var lastUpdate = -1
  }

  final class FactorType(val pot: Pot) extends Factor {
    var lastUpdate: Int            = -1
    var lastVector: FactorieVector = null
    var mean      : FactorieVector = null
  }


  def createFactor(pot: Pot) = new FactorType(pot)

  def createDiscNodeContent(variable: DiscVar[Any]) = new DiscNodeContent(variable.dom.size)
  def createContNodeContent(contVar: ContVar) = new ContNodeContent

  def inferMarginals(samples: Int, burnIn:Int = 0, weights: FactorieVector) = {

    //initialize
    for (n <- discNodes) {
      n.setting = random.nextInt(n.variable.dom.size)
      n.content.lastUpdate = 0
    }

    //burnin
    for (sample <- 0 until burnIn) {
      for (n <- discNodes; if !n.observed) {
        sampleDiscNode(weights, n, sample, true)
      }
    }

    //sample
    for (sample <- 0 until samples) {
      for (n <- discNodes; if !n.observed) {
        sampleDiscNode(weights, n, sample)
      }
    }
    //how to get partition function???
    //http://www.cc.gatech.edu/~mihail/D.lectures/jerrum96markov.pdf
    //make sure expectations are updated and consistent
    for (n <- discNodes) syncAverage(n, samples - 1)
    for (f <- factors; if f.pot.isLinear) syncStats(samples - 1, f)

    val marginals = var2DiscNode.values.map(n => DiscBelief(n.variable) -> Distribution.disc(n.variable.dom, n.content.belief))
    val gradient = new SparseVector(1000)
    for (f <- factors; if f.pot.isLinear) gradient += f.mean
    MarginalResult(0.0, gradient, new MapBasedState(marginals.toMap))
  }

  def sampleDiscNode(weights: FactorieVector, n: DiscNode, sample: Int, burnIn:Boolean = false) {
    fill(n.content.probs, 0.0)
    val oldSetting = n.setting
    for (i <- 0 until n.variable.dom.size) {
      n.setting = i
      for (e <- n.edges)
        n.content.probs(i) += e.factor.pot.score(e.factor, weights)
    }
    expNormalize(n.content.probs)
    val newSetting = nextDiscrete(n.content.probs)
    if (!burnIn && newSetting != oldSetting) {
      syncAverage(n, sample)
      n.setting = newSetting
      for (edge <- n.edges; if edge.factor.pot.isLinear) {
        syncStats(sample, edge.factor)
      }
    }
  }

  def syncStats(sample: Int, factor: FactorType) {
    val newStats = factor.pot.statsForCurrentSetting(factor)
    factor.mean *= factor.lastUpdate
    factor.mean +=(newStats, sample - factor.lastUpdate)
    factor.mean *= 1.0 / sample
    factor.lastUpdate = sample
  }

  def syncAverage(n: DiscNode, sample: Int) {
    if (sample > n.content.lastUpdate) {
      for (i <- 0 until n.variable.dom.size) {
        val rate = if (i == n.setting) 1.0 else 0.0
        n.content.belief(i) = (n.content.belief(i) * n.content.lastUpdate + rate * (sample - n.content.lastUpdate)) / sample
      }
      n.content.lastUpdate = sample
    }
  }
}
