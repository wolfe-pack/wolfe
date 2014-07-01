package ml.wolfe

import ml.wolfe.fg.Junkify

import scala.language.postfixOps


/**
 * @author Sebastian Riedel
 */
object BeliefPropagation {

  import FactorGraph._
  import MoreArrayOps._

  def maxProduct(maxIteration: Int, schedule: Boolean = true)(fg: FactorGraph) = apply(fg, maxIteration, schedule, false)
  def sumProduct(maxIteration: Int, schedule: Boolean = true)(fg: FactorGraph) = apply(fg, maxIteration, schedule, true)

  var runCount = 0
  var totalTime:Long = 0
  /**
   * Runs some iterations of belief propagation.
   * @param fg the message passing graph to run
   * @param maxIteration maximum number of iterations.
   * @param schedule should edges be scheduled.
   */
  def apply(fg: FactorGraph, maxIteration: Int, schedule: Boolean = true, sum: Boolean = false) {
    //    val edges = if (canonical) fg.edges.sorted(FactorGraph.EdgeOrdering) else fg.edges
    val t = System.currentTimeMillis()
    val fg2 = if(fg.isLoopy) {
      val jt = Junkify(fg)
      println("Weights = " + fg.weights + ", Junction Tree=\n" + jt.toVerboseString()); jt
    } else fg

    println("Original Factor Graph=\n" + fg.toVerboseString())
    fg.displayAsGraph()
    fg2.displayAsGraph()
   /* println(fg.edges.size)
    println(fg.edges.map(_.msgs.asDiscrete.n2f.length).sum)

    println(fg2.edges.size)
    println(fg2.edges.map(_.msgs.asTuple.n2f.array.length).sum)*/
    val edges = if (schedule) MPSchedulerImpl.schedule(fg2) else fg2.edges

    for (i <- 0 until maxIteration) {
      for (edge <- edges) {
        for (other <- edge.f.edges; if other != edge) updateN2F(other)
        updateF2N(edge, sum)
      }
    }
    for (node <- fg2.nodes) updateBelief(node, sum)

    //calculate gradient and objective
    //todo this is not needed if we don't have linear factors. Maybe initial size should depend on number of linear factors
    fg.gradient = new SparseVector(1000)
    fg.value = featureExpectationsAndObjective(fg2, fg.gradient, sum)

    runCount = runCount + 1
    totalTime = totalTime + (System.currentTimeMillis()-t)
    println(s"Belief Propagation has run $runCount times, avg ${totalTime/runCount}ms")
    //println("Value = " + fg.value + ". Gradient = " + fg.gradient)
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







