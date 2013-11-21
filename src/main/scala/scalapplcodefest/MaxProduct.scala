package scalapplcodefest

import scalaxy.loops._
import java.util
import cc.factorie.maths.ArrayOps


/**
 * @author Sebastian Riedel
 */
object MaxProduct {

  import MessagePassingFactorGraph._

  def main(args: Array[String]) {
    val fg = new MessagePassingFactorGraph
    val f1 = fg.addFactor2(Array(Array(1.0, 2.0, 3.0), Array(4.0, 5.0, 6.0)))
    val n1 = fg.addNode(2)
    val n2 = fg.addNode(3)
    val e1 = fg.addEdge(f1, n1)
    val e2 = fg.addEdge(f1, n2)
    fg.build()

    MaxProduct.run(fg, 1)

    println(n1.b.mkString(" "))
    println(n2.b.mkString(" "))

  }

  /**
   * Runs some iterations of belief propagation.
   * @param fg the message passing graph to run
   * @param maxIteration maximum number of iterations.
   */
  def run(fg: MessagePassingFactorGraph, maxIteration: Int) {
    for (i <- 0 until maxIteration) {
      for (edge <- fg.edges) {
        updateN2F(edge)
        updateF2N(edge)
      }
    }
    for (node <- fg.nodes) updateBelief(node)
  }

  def objective(fg: MessagePassingFactorGraph): Double = {
    ???
  }

  /**
   * Accumulates the expectations of all feature vectors under the current model. In MaxProduce expectations
   * are based on the MAP distribution.
   * @param fg factor graph.
   * @param result vector to add results to.
   */
  def featureExpectationsAndObjective(fg: MessagePassingFactorGraph, result: SparseVector): Double = {
    var obj = 0.0
    for (factor <- fg.factors) {
      // 1) go over all states, find max
      var norm = Double.NegativeInfinity
      for (i <- 0 until factor.entryCount) {
        val setting = factor.settings(i)
        val score = penalizedScore(factor, i, setting)
        norm = math.max(score, norm)
      }

      obj += norm

      if (factor.typ == MessagePassingFactorGraph.FactorType.LINEAR) {

        // 2) count number of maximums
        var maxCount = 0
        for (i <- 0 until factor.entryCount) {
          val setting = factor.settings(i)
          val score = penalizedScore(factor, i, setting)
          if (score == norm) maxCount += 1
        }

        // 3) prob = 1/|maxs| for all maximums, add corresponding vector
        for (i <- 0 until factor.entryCount) {
          val setting = factor.settings(i)
          val score = penalizedScore(factor, i, setting)
          if (score == norm) {
            result +=(factor.stats(i), 1.0 / maxCount)
          }
        }

      }
    }
    obj
  }


  def penalizedScore(factor: MessagePassingFactorGraph.Factor, entry: Int, setting: Array[Int]): Double = {
    var score = factor.score(entry)
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
    util.Arrays.fill(edge.f2n, Double.MinValue)
    for (i <- 0 until factor.entryCount) {
      val setting = factor.settings(i)
      var score = factor.score(i)
      val varValue = setting(edge.indexInFactor)
      for (j <- 0 until factor.rank; if j != edge.indexInFactor) {
        score += factor.edges(j).n2f(setting(j))
      }
      edge.f2n(varValue) = math.max(score, edge.f2n(varValue))
    }
  }

  /**
   * Updates the message from a node to a factor.
   * @param edge the factor-node edge.
   */
  def updateN2F(edge: Edge) {
    val node = edge.n
    System.arraycopy(node.in, 0, edge.n2f, 0, edge.n2f.length)
    for (i <- 0 until node.dim) {
      for (e <- 0 until node.edges.length; if e != edge.indexInNode)
        edge.n2f(i) += node.edges(e).f2n(i)
    }
  }

  /**
   * Updates the belief (sum of incoming messages) at a node.
   * @param node the node to update.
   */
  def updateBelief(node: Node) {
    System.arraycopy(node.in, 0, node.b, 0, node.b.length)
    for (e <- 0 until node.edges.length)
      for (i <- 0 until node.dim)
        node.b(i) += node.edges(e).f2n(i)
  }



}







