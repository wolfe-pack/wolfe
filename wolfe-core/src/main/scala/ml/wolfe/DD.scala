package ml.wolfe

import ml.wolfe.FactorGraph.{Node, Factor}
import scalaxy.loops._

/**
 * Dual decomposition with projected subgradient, AD3, ..
 *
 * @author luke
 */
object DD {

  /** Alternating Directions Dual Decomposition **/
  def ad3(fg:FactorGraph, maxIteration: Int, stepSize: Double = 1, parallelize: Boolean = true):Unit = {
    println("AD3!")
    apply(fg, maxIteration, _ => stepSize, parallelize, ad3 = true)
  }



  /**
   * Run dual decomposition on a factor graph and sets the MAP assignment to the belief variable of each node in the
   * graph
   * @param fg The message passing factor graph
   * @param maxIteration The maximum number of iterations after which the algorithm gives up
   * @param stepSize A function that gives the step size at each iteration. Defaults to defaultStepSize
   * @param parallelize A flag that indicates that each factor's inference can be run in parallel. Defaults to true
   */
  def apply(fg: FactorGraph, maxIteration: Int, stepSize: Int => Double = t => 1 / math.sqrt(t + 1),
            parallelize: Boolean = true, ad3:Boolean = false):Unit = {
    val factors = if (parallelize) fg.factors.par else fg.factors

    fg.converged = false

    // not sure if we need this. But who knows where this graph came from
    initializeN2FMessages(fg)

    // scumbag scala! makes me write java
    var iter = 0
    while (iter < maxIteration && !fg.converged) {

      for(f <- factors) if(ad3) f.potential.quadraticProgramF2N(stepSize(iter), 1000) else f.potential.mapF2N()
      for(n <- fg.nodes) n.variable.updateMarginalBelief(n)
      for(n <- fg.nodes; e <- n.edges) n.variable.updateDualN2F(e, stepSize(iter))

      fg.converged = hasConverged(fg)

      iter = iter + 1
    }
  }


  /**
   * Initializes the messages from each node to a factor to true. These variables store the values of the dual
   * variables.
   * @param fg The factor graph
   */
  private def initializeN2FMessages(fg: FactorGraph): Unit = {
    for (factor <- fg.factors;
         edge <- factor.edges) {
      for (i <- 0 until edge.msgs.asDiscrete.n2f.size)
        edge.msgs.asDiscrete.n2f(i) = 0
    }
  }


  /**
   * Checks if for every factor, the nodes that are shared with another factor have consistent beliefs.
   *
   * @param fg The factor graph
   * @return
   */
  private def hasConverged(fg: FactorGraph): Boolean = {
    var hasConverged = true

    for (factor <- fg.factors; if hasConverged) {
      for (edge <- factor.edges;
           otherEdge <- edge.n.edges
           if otherEdge != edge
           if otherEdge.f != factor
           if hasConverged) {

        for (i <- 0 until edge.msgs.asDiscrete.dim; if hasConverged) {
          hasConverged = edge.msgs.asDiscrete.f2n(i) == otherEdge.msgs.asDiscrete.f2n(i)
        }
      }
    }
    hasConverged
  }
}