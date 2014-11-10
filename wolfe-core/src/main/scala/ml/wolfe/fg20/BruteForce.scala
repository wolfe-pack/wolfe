package ml.wolfe.fg20

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

  def inferMAP(weights: FactorieVector): MAPResult = {
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
    for (f <- factors)
      gradient += f.pot.statsForCurrentSetting(f)

    val state = nodes.map(n => n.variable -> n.variable.dom(n.content.setting))
    val maxMarginals = nodes.map(n => DiscBelief(n.variable) -> Distribution.disc(n.variable.dom,n.content.belief))
    MAPResult(new MapBasedState(state.toMap), maxScore, gradient, new MapBasedState(maxMarginals.toMap))
  }

  def marginalize(weights: FactorieVector): MarginalResult = {
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
      for (f <- factors)
        gradient +=(f.pot.statsForCurrentSetting(f), prob)
    }

    val marginals = nodes.map(n => DiscBelief(n.variable) -> Distribution.disc(n.variable.dom,n.content.belief))
    MarginalResult(logZ,gradient,new MapBasedState(marginals.toMap))
  }


}

/**
 * Searches through all states of the factor graph.
 */
object BruteForce {

  trait Potential extends ml.wolfe.fg20.Potential {
    def score(factor: BruteForce#Factor, weights: FactorieVector): Double
    def statsForCurrentSetting(factor: BruteForce#Factor): FactorieVector
  }

  import ml.wolfe.FactorGraph._

  def loopOverSettings(nodes: List[Node], loop: (() => Unit) => Unit = body => body()): (() => Unit) => Unit = {
    nodes match {
      case Nil => (body: () => Unit) => loop(body)
      case head :: tail =>
        def newLoop(body: () => Unit) {
          for (setting <- head.variable.asDiscrete.domain.indices) {
            head.variable.asDiscrete.setting = setting
            loop(body)
          }
        }
        loopOverSettings(tail, newLoop)
    }
  }

  def maxMarginals(fg: FactorGraph) {
    val loop = loopOverSettings(fg.nodes.toList)
    var maxScore = Double.NegativeInfinity
    for (n <- fg.nodes) n.variable.initializeToNegInfinity()

    var maxSetting: Array[Int] = null
    loop { () =>
      val score = currentScore(fg)
      for (n <- fg.nodes) {
        val v = n.variable.asDiscrete
        v.b(v.setting) = math.max(score, v.b(v.setting))
      }

      if (score > maxScore) {
        maxScore = score
        maxSetting = fg.nodes.view.map(_.variable.asDiscrete.setting).toArray
      }
    }

    for ((s, n) <- maxSetting zip fg.nodes) {
      val v = n.variable.asDiscrete
      v.setting = s
      maxNormalize(v.b)
    }

    fg.value = maxScore
    fg.gradient = new SparseVector(1000)

    for (f <- fg.factors; if f.potential.isLinear)
      fg.gradient += f.potential.statsForCurrentSetting()

    //println("Bruteforce: " + maxScore)

  }

  def currentScore(fg: FactorGraph) = {
    var score = 0.0
    var i = 0
    while (i < fg.factors.size) {
      score += fg.factors(i).potential.valueForCurrentSetting()
      i += 1
    }
    score
  }

  def marginalize(fg: FactorGraph) {
    val loop = loopOverSettings(fg.nodes.toList)
    var Z = 0.0
    for (n <- fg.nodes) fill(n.variable.asDiscrete.b, 0.0) //n.variable.initializeToNegInfinity()

    loop { () =>

      val score = currentScore(fg)
      val prob = math.exp(score)
      for (n <- fg.nodes) {
        val v = n.variable.asDiscrete
        v.b(v.setting) = v.b(v.setting) + prob //math.max(score,v.b(v.setting))
      }
      Z += prob
    }

    for (n <- fg.nodes) {
      val v = n.variable.asDiscrete
      normalize(v.b)
      log(v.b)
    }

    fg.value = math.log(Z)
    fg.gradient = new SparseVector(1000)

    loop { () =>
      val score = currentScore(fg)
      val prob = math.exp(score) / Z
      for (f <- fg.factors; if f.potential.isLinear)
        fg.gradient +=(f.potential.statsForCurrentSetting(), prob)
    }


    //println("Bruteforce: " + maxScore)

  }

}
