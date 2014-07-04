package ml.wolfe

import cc.factorie.la.SparseIndexedTensor
import ml.wolfe.fg.Junkify
import ml.wolfe.util.LoggerUtil

import scala.language.postfixOps


/**
 * @author Sebastian Riedel
 */
object BeliefPropagation {

  import FactorGraph._
  import MoreArrayOps._

  def maxProduct(maxIteration: Int, schedule: Boolean = true, gradientAndObjective: Boolean = true)
                (fg: FactorGraph) = apply(fg, maxIteration, schedule, false, gradientAndObjective)
  def sumProduct(maxIteration: Int, schedule: Boolean = true, gradientAndObjective: Boolean = true)
                (fg: FactorGraph) = apply(fg, maxIteration, schedule, true, gradientAndObjective)
  def junctionTreeMaxProduct(gradientAndObjective: Boolean = true)
                (fg: FactorGraph) = onJunctionTree(fg, sum=false, gradientAndObjective=gradientAndObjective)
  def junctionTreeSumProduct(gradientAndObjective: Boolean = true)
                (fg: FactorGraph) = onJunctionTree(fg, sum=true, gradientAndObjective=gradientAndObjective)

  /**
   * Runs some iterations of belief propagation.
   * @param fg the message passing graph to run
   * @param maxIteration maximum number of iterations.
   * @param schedule should edges be scheduled.
   */
  def apply(fg: FactorGraph, maxIteration: Int,
            schedule: Boolean = true,
            sum: Boolean = false,
            gradientAndObjective: Boolean = true) {
    //    val edges = if (canonical) fg.edges.sorted(FactorGraph.EdgeOrdering) else fg.edges
    val edges = if (schedule) MPSchedulerImpl.schedule(fg) else fg.edges

    for (i <- 0 until maxIteration) {
      for (edge <- edges) {
        for (other <- edge.f.edges; if other != edge) updateN2F(other) //todo: this is inefficient! Don't need to update if unchanged!
        updateF2N(edge, sum)
      }
    }

    for (node <- fg.nodes) updateBelief(node, sum)

    //calculate gradient and objective
    //todo this is not needed if we don't have linear factors. Maybe initial size should depend on number of linear factors
    if (gradientAndObjective) {
      fg.gradient = new SparseVector(1000)
      fg.value = featureExpectationsAndObjective(fg, fg.gradient, sum)
    }

  }

  /**
   *  Creates a junction tree from the original factor graph, runs BP,
   *  then copies value/gradient back into the original factor graph.
   *  @param fg the original factor graph
   */

  def onJunctionTree(fg: FactorGraph, sum: Boolean = false, gradientAndObjective: Boolean = true, forceJunctionTree:Boolean = false) {
    if(!forceJunctionTree && !fg.isLoopy) {
      LoggerUtil.once(LoggerUtil.warn, "Junction tree belief propagation called on a non-loopy graph. Ignoring.")
      apply(fg, 1, schedule=true, sum, gradientAndObjective)
    } else {
      val jt = Junkify(fg)
      apply(jt, 1, schedule=true, sum, gradientAndObjective)
      if (gradientAndObjective) {
        fg.value = jt.value
        fg.gradient = jt.gradient
      }
    }
  }

  /**
   * Accumulates the expectations of all feature vectors under the current model. In MaxProduce expectations
   * are based on the MAP distribution.
   * @param fg factor graph.
   * @param result vector to add results to.
   */
  def featureExpectationsAndObjective(fg: FactorGraph, result: FactorieVector, sum: Boolean): Double = {
    var obj = 0.0
    for (factor <- fg.factors) {
      //update all n2f messages
      for (e <- factor.edges) updateN2F(e)
      obj += {
        if (sum) factor.potential.marginalExpectationsAndObjective(result)
        else factor.potential.maxMarginalExpectationsAndObjective(result)
      }
    }
    //in case we do sum-product we need to subtract doubly counted node entropies to calculate the bethe objective.
    if (sum) for (node <- fg.nodes) {
      obj += (1.0 - node.edges.size) * node.variable.entropy()
    }
    obj
  }

  /**
   * Updates the message from factor to node.
   * @param edge the factor-node edge.
   */
  def updateF2N(edge: Edge, sum: Boolean) {
    val factor = edge.f

    //remember last message for calculating residuals
    edge.msgs.saveCurrentF2NAsOld()

    //message calculation happens in potential
    if (sum) factor.potential.marginalF2N(edge) else factor.potential.maxMarginalF2N(edge)

  }

  /**
   * Updates the message from a node to a factor.
   * @param edge the factor-node edge.
   */
  def updateN2F(edge: Edge) {
    edge.n.variable.updateN2F(edge)
  }

  /**
   * Updates the belief (sum of incoming messages) at a node.
   * @param node the node to update.
   */
  def updateBelief(node: Node, sum: Boolean) {
    if (sum) node.variable.updateMarginalBelief(node) else node.variable.updateMaxMarginalBelief(node)
  }


}







