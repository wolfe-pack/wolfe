package ml.wolfe.fg20

import cc.factorie.maths._
import ml.wolfe.MoreArrayOps._
import ml.wolfe.{FactorieVector, SparseVector}

import scala.util.Random

/**
 * @author Sebastian Riedel
 */
class GibbsSampler(val problem: Problem)
                  (implicit random: Random = new Random(0)) extends FactorGraph with NodeContentFactorGraph with EmptyEdgeFactorGraph  {

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


  final class VectNodeContent() extends NodeContent {
    var mean: FactorieVector = null
  }

  final class FactorType(val pot: Pot) extends Factor {
    var mean: FactorieVector = null
    var setting              = pot.createSetting()
  }


  type Pot = Potential

  def acceptPotential = { case p: Potential => p }
  def createFactor(pot: Pot) = new FactorType(pot)

  def createDiscNodeContent(variable: DiscVar[Any]) = new DiscNodeContent(variable.dom.size)
  def createContNodeContent(contVar: ContVar) = new ContNodeContent
  def createVectNodeContent(vectVar: VectVar) = new VectNodeContent

  def inferMarginals(samples: Int, burnIn: Int = 0) = {

    syncFactorObservations()

    //initialize
    for (n <- discNodes) {
      n.setting = random.nextInt(n.variable.dom.size)
      n.content.lastUpdate = 0
    }

    //burnin
    for (sample <- 0 until burnIn) {
      for (n <- discNodes; if !n.observed) {
        sampleDiscNode(n, sample, burnIn = true)
      }
    }

    //sample
    for (sample <- 0 until samples) {
      for (n <- discNodes; if !n.observed) {
        sampleDiscNode(n, sample)
      }
      for (f <- factors) {
        f.processor match { case p: ExpFamProcessor => addStats(sample, f, p) case _ => }
      }
    }
    //how to get partition function???
    //http://www.cc.gatech.edu/~mihail/D.lectures/jerrum96markov.pdf
    //make sure expectations are updated and consistent
    for (n <- discNodes) syncAverage(n, samples)
    for (f <- factors) f.processor match { case p: ExpFamProcessor => addStats(samples - 1, f, p) case _ => }

    val marginals = var2DiscNode.values.map(n => DiscBelief(n.variable) -> Distribution.disc(n.variable.dom, n.content.belief))
    val gradient = new SparseVector(1000)
    for (f <- factors; if f.mean != null) gradient += f.mean
    MarginalResult(0.0, gradient, new MapBasedState(marginals.toMap))
  }

  def sampleDiscNode(n: DiscNode, sample: Int, burnIn: Boolean = false) {
    fill(n.content.probs, 0.0)
    val oldSetting = n.setting
    for (i <- 0 until n.variable.dom.size) {
      n.setting = i
      for (e <- n.edges) {
        e.factor.setting.disc(e.index) = i
        n.content.probs(i) += e.factor.processor.score(e.factor.setting)
      }
    }
    n.setting = oldSetting
    expNormalize(n.content.probs)
    val newSetting = nextDiscrete(n.content.probs)
    if (newSetting != oldSetting) {
      if (!burnIn) syncAverage(n, sample)
      n.setting = newSetting
      for (e <- n.edges) {
        e.factor.setting.disc(e.index) = newSetting
      }

    }
  }


  //f = (f' * (n-1) + newStats) / n = f' * (n-1)/n + newStats / n
  def addStats(sample: Int, factor: FactorType, expFamProc: ExpFamProcessor) {
    val newStats = expFamProc.stats(factor.setting)
    if (sample == 0) factor.mean = new SparseVector(0)
    factor.mean *= sample / (sample + 1)
    factor.mean +=(newStats, 1.0 / (sample + 1))
  }

  //a = (a' * (lastUpdate) + (now - lastUpdate) * newStats) / now =  a' * lastUpdate/now + (now-lastUpdate)/now * newValue
  def syncAverage(n: DiscNode, sample: Int) {
    if (sample > n.content.lastUpdate) {
      for (i <- 0 until n.variable.dom.size) {
        n.content.belief(i) *= n.content.lastUpdate.toDouble / sample
        if (i == n.setting)
          n.content.belief(i) += (sample - n.content.lastUpdate).toDouble / sample
      }
      n.content.lastUpdate = sample
    }
  }

  def syncFactorObservations(): Unit = {
    for (n <- discNodes; if n.observed; e <- n.edges) e.factor.setting.disc(e.index) = n.setting
    for (c <- contNodes; if c.observed; e <- c.edges) e.factor.setting.cont(e.index) = c.setting
    for (v <- vectNodes; if v.observed; e <- v.edges) e.factor.setting.vect(e.index) = v.setting
  }

}
