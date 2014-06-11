package ml.wolfe

import scala.language.postfixOps
import ml.wolfe.MoreArrayOps._

/**
 * Searches through all states of the factor graph.
 */
object BruteForceSearch {
  def apply(fg: FactorGraph) {
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
    val loop = loopOverSettings(fg.nodes.toList)
    var maxScore = Double.NegativeInfinity
    for (n <- fg.nodes) n.variable.initializeToNegInfinity()

    var maxSetting: Array[Int] = null
    loop { () =>
      var score = 0.0
      var i = 0
      while (i < fg.factors.size) {
        score += fg.factors(i).potential.valueForCurrentSetting()
        i += 1
      }
      for (n <- fg.nodes) {
        val v = n.variable.asDiscrete
        v.b(v.setting) = math.max(score,v.b(v.setting))
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
      fg.gradient += f.potential.stats()

    //println("Bruteforce: " + maxScore)

  }
}
