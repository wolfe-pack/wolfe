package ml.wolfe

import scala.language.postfixOps


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
   * @param canonical should edges be processed in canonical ordering according.
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
   * Updates the message from factor to node.
   * @param edge the factor-node edge.
   */
  def updateF2N(edge: Edge) {
    val factor = edge.f

    //remember last message for calculating residuals
    edge.msgs.saveCurrentFN2AsOld()

    //message calculation happens in potential
    factor.potential.maxMarginalF2N(edge)

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
  def updateBelief(node: Node) {
    node.variable.updateBelief(node)
  }


}







