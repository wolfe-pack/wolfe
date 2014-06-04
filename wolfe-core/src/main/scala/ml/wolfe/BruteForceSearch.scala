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
            for (setting <- head.domain.indices) {
              head.setting = setting
              loop(body)
            }
          }
          loopOverSettings(tail, newLoop)
      }
    }
    val loop = loopOverSettings(fg.nodes.toList)
    var maxScore = Double.NegativeInfinity
    for (n <- fg.nodes) fill(n.b, Double.NegativeInfinity)

    var maxSetting: Array[Int] = null
    loop { () =>
      var score = 0.0
      var i = 0
      while (i < fg.factors.size) {
        score += fg.factors(i).potential.value()
        i += 1
      }
      for (n <- fg.nodes) {
        n.b(n.setting) = math.max(score,n.b(n.setting))
      }

      if (score > maxScore) {
        maxScore = score
        maxSetting = fg.nodes.view.map(_.setting).toArray
      }
    }

    for ((s, n) <- maxSetting zip fg.nodes) {
      n.setting = s
      maxNormalize(n.b)
    }

    fg.value = maxScore
    fg.gradient = new SparseVector(1000)

    for (f <- fg.factors; if f.potential.isLinear)
      fg.gradient += f.potential.stats()

    //println("Bruteforce: " + maxScore)

  }
}
