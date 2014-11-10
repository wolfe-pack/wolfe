package ml.wolfe.fg20

import cc.factorie.la.DenseTensor1
import ml.wolfe.{FactorieVector, SparseVector, FactorGraph}
import ml.wolfe.MoreArrayOps._


trait NoMsgs extends FG {
  class DiscMsgs extends Msgs
  class ContMsgs extends Msgs
  class Msgs

  val emptyDiscMsgs = new DiscMsgs
  val emptyContMsgs = new ContMsgs

  def createDiscMsgs(variable: DiscVar[Any]) = emptyDiscMsgs
  def createContMsgs(contVar: ContVar) = emptyContMsgs
}

trait NoFactorContent extends FG {
  class FactorContent
  val empty = new FactorContent
  def createFactorContent(pot: Pot) = empty
}

class BruteForce(val problem: Problem) extends NoMsgs with NoFactorContent {
  class DiscNodeContent(var setting: Int, var belief: Array[Double])
  type ContNodeContent = Nothing
  def createDiscNodeContent(variable: DiscVar[Any]) = new DiscNodeContent(0, Array.ofDim[Double](variable.dom.size))
  def createContNodeContent(contVar: ContVar) = sys.error("Can't do brute force with continuous variables")
  def acceptPotential = { case p: BruteForce.Potential => p }
  type Pot = BruteForce.Potential

  def loopOverSettings(nodes: List[DiscNode], loop: (() => Unit) => Unit = body => body()): (() => Unit) => Unit = {
    nodes match {
      case Nil => (body: () => Unit) => loop(body)
      case head :: tail =>
        def newLoop(body: () => Unit) {
          for (setting <- head.variable.dom.indices) {
            head.content.setting = setting
            loop(body)
          }
        }
        loopOverSettings(tail, newLoop)
    }
  }

  def currentScore(weights: FactorieVector) = {
    factors.view.map(f => f.pot.score(f, weights)).sum
  }

  def inferMAP(weights: FactorieVector = new DenseTensor1(0)): MAPResult = {
    val nodes = discNodes.values.toList
    val loop = loopOverSettings(nodes)
    var maxScore = Double.NegativeInfinity
    for (n <- nodes) fill(n.content.belief, Double.NegativeInfinity)

    var maxSetting: Array[Int] = null
    loop { () =>
      val score = currentScore(weights)
      for (n <- nodes) {
        n.content.belief(n.content.setting) = math.max(score, n.content.belief(n.content.setting))
      }

      if (score > maxScore) {
        maxScore = score
        maxSetting = nodes.view.map(_.content.setting).toArray
      }
    }

    for ((s, n) <- maxSetting zip nodes) {
      n.content.setting = s
      maxNormalize(n.content.belief)
    }

    val gradient = new SparseVector(1000)
    for (f <- factors; if f.pot.isLinear)
      gradient += f.pot.statsForCurrentSetting(f)

    val state = nodes.map(n => n.variable -> n.variable.dom(n.content.setting))
    val maxMarginals = nodes.map(n => DiscBelief(n.variable) -> Distribution.disc(n.variable.dom, n.content.belief))
    MAPResult(new MapBasedState(state.toMap), maxScore, gradient, new MapBasedState(maxMarginals.toMap))
  }

  def inferMarginals(weights: FactorieVector = new DenseTensor1(0)): MarginalResult = {
    val nodes = discNodes.values.toList
    val loop = loopOverSettings(nodes)
    var Z = 0.0
    for (n <- nodes) fill(n.content.belief, 0.0)

    loop { () =>
      val score = currentScore(weights)
      val prob = math.exp(score)
      for (n <- nodes) {
        n.content.belief(n.content.setting) += prob
      }
      Z += prob
    }

    for (n <- nodes) {
      normalize(n.content.belief)
      log(n.content.belief)
    }

    val logZ = math.log(Z)
    val gradient = new SparseVector(1000)

    loop { () =>
      val score = currentScore(weights)
      val prob = math.exp(score) / Z
      for (f <- factors; if f.pot.isLinear)
        gradient +=(f.pot.statsForCurrentSetting(f), prob)
    }

    val marginals = nodes.map(n => DiscBelief(n.variable) -> Distribution.disc(n.variable.dom, n.content.belief))
    MarginalResult(logZ, gradient, new MapBasedState(marginals.toMap))
  }


}

/**
 * Searches through all states of the factor graph.
 */
object BruteForce {

  trait Potential extends ml.wolfe.fg20.DiscPotential {
    def score(factor: BruteForce#Factor, weights: FactorieVector): Double
    def statsForCurrentSetting(factor: BruteForce#Factor): FactorieVector
  }

  import ml.wolfe.FactorGraph._


}
