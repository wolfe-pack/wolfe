package ml.wolfe

import scalaxy.loops._
import scala.language.postfixOps
import ml.wolfe.MoreArrayOps._


/**
 * @author Sebastian Riedel
 */
object MaxProduct {

  import FactorGraph._
  import MoreArrayOps._

  /**
   * Runs some iterations of belief propagation.
   * @param fg the message passing graph to run
   * @param maxIteration maximum number of iterations.
   * @param canonical should edges be processed in canonical ordering according to [[ml.wolfe.FactorGraph.EdgeOrdering]].
   */
  def apply(fg: FactorGraph, maxIteration: Int, canonical: Boolean = true) {
    //    val edges = if (canonical) fg.edges.sorted(FactorGraph.EdgeOrdering) else fg.edges
    val edges = if (canonical) MPSchedulerImpl.schedule(fg) else fg.edges

    for (i <- 0 until maxIteration) {
      for (edge <- edges) {
        for (other <- edge.f.edges; if other != edge) updateN2F(other)
        updateF2N(edge)
      }
    }
    for (node <- fg.nodes) updateBelief(node)

    //calculate gradient and objective
    //todo this is not needed if we don't have linear factors. Maybe initial size should depend on number of linear factors
    fg.gradient = new SparseVector(1000)
    fg.value = featureExpectationsAndObjective(fg, fg.gradient)

  }


  /**
   * Accumulates the expectations of all feature vectors under the current model. In MaxProduce expectations
   * are based on the MAP distribution.
   * @param fg factor graph.
   * @param result vector to add results to.
   */
  def featureExpectationsAndObjective(fg: FactorGraph, result: FactorieVector): Double = {
    var obj = 0.0
    for (factor <- fg.factors) {
      //update all n2f messages
      for (e <- factor.edges) updateN2F(e)
      obj += factor.potential.maxMarginalExpectationsAndObjective(result)
    }
    obj
  }

  /**
   * Calculates the score of a setting and adds penalties based on incoming messages of the factor.
   * @param factor the factor to calculate the penalised score for.
   * @param settingId id of the setting to score.
   * @param setting the setting corresponding to the id.
   * @return penalized score of setting.
   */
  def penalizedScore(factor: FactorGraph.Factor, settingId: Int, setting: Array[Int]): Double = {
    var score = factor.score(settingId)
    for (j <- 0 until factor.rank) {
      score += factor.edges(j).n2f(setting(j))
    }
    score
  }

  /**
   * Updates the message from factor to node.
   * @param edge the factor-node edge.
   */
  def updateF2N(edge: Edge) {
    val factor = edge.f

    //remember last message for calculating residuals
    set(edge.f2n, edge.f2nLast)

    //message calculation happens in potential
    factor.potential.maxMarginalF2N(edge)

  }

  /**
   * Updates the message from a node to a factor.
   * @param edge the factor-node edge.
   */
  def updateN2F(edge: Edge) {
    val node = edge.n
    System.arraycopy(node.in, 0, edge.n2f, 0, edge.n2f.length)
    for (i <- (0 until node.dim).optimized) {
      for (e <- (0 until node.edges.length).optimized; if e != edge.indexInNode)
        edge.n2f(i) += node.edges(e).f2n(i)
    }
  }

  /**
   * Updates the belief (sum of incoming messages) at a node.
   * @param node the node to update.
   */
  def updateBelief(node: Node) {
    System.arraycopy(node.in, 0, node.b, 0, node.b.length)
    for (e <- 0 until node.edges.length) {
      for (i <- 0 until node.dim)
        node.b(i) += node.edges(e).f2n(i)
      maxNormalize(node.b)
    }
  }


}

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





