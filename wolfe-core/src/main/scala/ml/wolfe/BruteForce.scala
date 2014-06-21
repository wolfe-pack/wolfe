package ml.wolfe

import ml.wolfe.FactorGraph.Node

import scala.language.postfixOps
import ml.wolfe.MoreArrayOps._

/**
 * Searches through all states of the factor graph.
 */
object BruteForce {

  import FactorGraph._

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
