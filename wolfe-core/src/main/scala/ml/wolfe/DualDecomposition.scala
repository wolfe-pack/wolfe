package ml.wolfe

import ml.wolfe.FactorGraph.{Node, Factor}

/**
 * Run dual decomposition on a message passing graph.
 *
 * @author svivek
 */
object DualDecomposition {

  /**
   * The default step size for a given dual decomposition iteration.
   * @param i The iteration
   * @return
   */
  def defaultStepSize(i: Int) = 1 / math.sqrt(i + 1)

  /**
   * Run dual decomposition on a factor graph and sets the MAP assignment to the belief variable of each node in the
   * graph
   * @param fg The message passing factor graph
   * @param maxIteration The maximum number of iterations after which the algorithm gives up
   * @param stepSize A function that gives the step size at each iteration. Defaults to defaultStepSize
   * @param parallelize A flag that indicates that each factor's inference can be run in parallel. Defaults to true
   */
  def apply(fg: FactorGraph, maxIteration: Int, stepSize: Int => Double = defaultStepSize, parallelize: Boolean = true):
  Unit = {

    val factors = if (parallelize) fg.factors.par else fg.factors

    fg.converged = false

    // not sure if we need this. But who knows where this graph came from
    initializeN2FMessages(fg)

    // scumbag scala! makes me write java
    var iter = 0
    while (iter < maxIteration && !fg.converged) {

      factors foreach solveFactorWithPenalty

      fg.converged = hasConverged(fg)

      updatePenalties(fg, stepSize(iter))

      iter = iter + 1
    }

    // whether it has converged or not, set the belief. If it hasn't converged, we will still have an output; it won't
    // mean anything though
    for (node <- fg.nodes) updateBelief(node)
  }


  /**
   * Initializes the messages from each node to a factor to true. These variables store the values of the dual
   * variables.
   * @param fg The factor graph
   */
  private def initializeN2FMessages(fg: FactorGraph): Unit = {
    for (factor <- fg.factors;
         edge <- factor.edges) {
      for (i <- 0 until edge.n2f.size)
        edge.n2f(i) = 0
    }
  }

  /**
   * Find the score-maximizing assignment to the nodes associated with a factor, accounting for the score penalties
   * @param factor The factor
   */
  def solveFactorWithPenalty(factor: FactorGraph.Factor): Unit = {
    factor.typ match {
      case FactorGraph.FactorType.TABLE =>
        val best = (0 until factor.table.size).maxBy(i => penalizedScore(factor, i))
        propagateSettingInformation(factor, factor.settings(best))
      case FactorGraph.FactorType.LINEAR =>
        val best = (0 until factor.table.size).maxBy(i => penalizedScore(factor, i))
        propagateSettingInformation(factor, factor.settings(best))
      case FactorGraph.FactorType.STRUCTURED =>
        // TODO: How does one mandate that this uses the penalties
        val argmax = factor.structured.argmaxMarginal2AllNodes(factor)

        propagateSettingInformation(factor, argmax)
    }
  }

  /**
   * For each node associated with the factor, send a message that identifies its best setting as a one-hot vector
   * @param factor The factor graph
   * @param setting The best setting for the nodes connected to this factor
   */
  def propagateSettingInformation(factor: FactorGraph.Factor, setting: Array[Int]) = {

    for (i <- 0 until factor.edges.length) {
      val edge = factor.edges(i)
      val s = setting(i)

      // clear out all old information
      for (j <- 0 until edge.f2n.size)
        edge.f2n(j) = 0

      // set the new piece of information
      edge.f2n(s) = 1
    }
  }

  /**
   * Calculates the score of a setting and adds penalties based on incoming messages of the factor.
   * @param factor the factor to calculate the penalised score for.
   * @param settingId id of the setting to score.
   * @return penalized score of setting.
   */
  def penalizedScore(factor: FactorGraph.Factor, settingId: Int): Double = {
    var score = factor.score(settingId)

    val setting = factor.settings(settingId)

    for (j <- 0 until factor.rank) {
      score += factor.edges(j).n2f(setting(j))
    }
    score
  }

  /**
   * Update all penalties for the factor graph. This is equivalent to the gradient descent step in the
   * dual-decomposition algorithm
   * @param fg The factor graph
   * @param stepSize The step size for the gradient descent
   */
  def updatePenalties(fg: FactorGraph, stepSize: Double): Unit = {
    fg.factors.foreach {updateFactorPenalties(_, stepSize)}
  }

  def updateFactorPenalties(factor: Factor, stepSize: Double): Unit = {
    for (edge <- factor.edges) {
      val node = edge.n

      for (otherEdge <- node.edges
           if otherEdge != edge) {
        for (i <- 0 until edge.f2n.size) {
          edge.n2f(i) -= stepSize * (edge.f2n(i) - otherEdge.f2n(i))
        }
      }


    }
  }

  /**
   * Checks if for every factor, the nodes that are shared with another factor have consistent beliefs.
   *
   * @param fg The factor graph
   * @return
   */
  def hasConverged(fg: FactorGraph): Boolean = {
    var hasConverged = true

    for (factor <- fg.factors; if hasConverged) {
      for (edge <- factor.edges;
           otherEdge <- edge.n.edges
           if otherEdge != edge
           if otherEdge.f != factor
           if hasConverged) {

        for (i <- 0 until edge.dim; if hasConverged) {
          hasConverged = edge.f2n(i) == otherEdge.f2n(i)
        }
      }
    }
    hasConverged
  }

  /**
   * Updates the belief (sum of incoming messages) at a node.
   * @param node the node to update.
   */
  def updateBelief(node: Node) {
    System.arraycopy(node.in, 0, node.b, 0, node.b.length)
    for (e <- 0 until node.edges.length) {
      for (i <- 0 until node.dim) {
        node.b(i) += node.edges(e).f2n(i)
      }
    }
  }
}