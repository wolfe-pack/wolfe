package ml.wolfe

import cc.factorie.la.SparseIndexedTensor
import ml.wolfe.fg.{TupleMsgs, DiscreteMsgs, DiscreteVar, Junkify}
import ml.wolfe.util.LoggerUtil


/**
 * @author Sebastian Riedel
 */
object BeliefPropagation {

  import FactorGraph._
  import MoreArrayOps._

  object BPType extends Enumeration {
    type BPType = Value
    /** Computes marginals **/
    val Sum = Value
    /** Computes max-marginals and sets setting to a valid argmax **/
    val Max = Value
    /** Computes max-marginals ONLY (setting may not be a valid argmax if there are multiple solutions) **/
    val MaxMarginals = Value
    /** Sets setting to a valid argmax **/
    val MaxOnly = Value
  }

  object BPSchedule extends Enumeration {
    type BPSchedule = Value
    val Auto = Value
    val Canonical = Value
    val Pow = Value
  }

  import BPType._
  import BPSchedule._

  def maxProduct(maxIteration: Int, schedule: BPSchedule = Auto, gradientAndObjective: Boolean = true)
                (fg: FactorGraph) = apply(fg, maxIteration, schedule, Max, gradientAndObjective)
  def sumProduct(maxIteration: Int, schedule: BPSchedule = Auto, gradientAndObjective: Boolean = true)
                (fg: FactorGraph) = apply(fg, maxIteration, schedule, Sum, gradientAndObjective)
  def maxProductPow(maxIteration: Int, schedule: BPSchedule = Pow, gradientAndObjective: Boolean = true)
                (fg: FactorGraph) = apply(fg, maxIteration, schedule, Max, gradientAndObjective)
  def sumProductPow(maxIteration: Int, schedule: BPSchedule = Pow, gradientAndObjective: Boolean = true)
                (fg: FactorGraph) = apply(fg, maxIteration, schedule, Sum, gradientAndObjective)
  def junctionTreeMaxProduct(gradientAndObjective: Boolean = true)
                (fg: FactorGraph) = onJunctionTree(fg, Sum, gradientAndObjective = gradientAndObjective)
  def junctionTreeSumProduct(gradientAndObjective: Boolean = true)
                (fg: FactorGraph) = onJunctionTree(fg, Max, gradientAndObjective = gradientAndObjective)

  /**
   * Runs some iterations of belief propagation.
   * @param fg the message passing graph to run
   * @param maxIteration maximum number of iterations.
   * @param schedule should edges be scheduled.
   */
  def apply(fg: FactorGraph, maxIteration: Int,
            schedule: BPSchedule = Auto,
            bpType:BPType = BPType.Max,
            gradientAndObjective: Boolean = true) {

    val forwardEdges = schedule match {
      case Auto => MPSchedulerImpl.scheduleForward(fg)
      case Canonical => MPSchedulerImpl.canonicalSchedule(fg)
      case Pow => Nil
    }
    val backwardEdges = forwardEdges.reverse.map(_.swap)
    val forwardBackwardEdges = forwardEdges ++ backwardEdges

    val convergenceThreshold = 1e-6 * fg.nodes.map( n =>
      n.variable match {case d:DiscreteVar[_] => d.dim  * n.edges.length case _ => 1} ).sum
    var hasConverged = false

    var i = 0
    while (!hasConverged && (i < maxIteration || maxIteration == -1)) {

      if(schedule == Pow) {
        for(f <- fg.factors) powF2N(f, bpType == Sum)
        for(n <- fg.nodes) powN2F(n)
      } else {
        def edges = if (bpType == MaxOnly && i == maxIteration - 1) forwardEdges else forwardBackwardEdges
        for (e <- forwardBackwardEdges) e match {
          case DirectedEdge(edge, EdgeDirection.F2N) => updateF2N(edge, bpType == Sum)
          case DirectedEdge(edge, EdgeDirection.N2F) => updateN2F(edge)
        }
      }

      hasConverged = converged(fg, convergenceThreshold)
      i = i + 1
    }

    if(bpType == Sum) for (node <- fg.nodes if !node.variable.isObserved) updateBelief(node, true)
    if(bpType == Max || bpType == MaxMarginals) for (node <- fg.nodes if !node.variable.isObserved) updateBelief(node, false)


    //calculate gradient and objective
    //todo this is not needed if we don't have linear factors. Maybe initial size should depend on number of linear factors
    if (gradientAndObjective) {
      fg.gradient = new SparseVector(1000)
      fg.value = featureExpectationsAndObjective(fg, fg.gradient, bpType==Sum)
    }

    //calculate expectations
    if (fg.expectationFactors.size > 0) calculateExpectations(fg, bpType == Sum)

    if(bpType == Max || bpType == MaxOnly) {
      if(schedule == Pow) ???
      else {
        for (e <- backwardEdges) {
          e match {
            case DirectedEdge(edge, EdgeDirection.F2N) => updateF2N(edge, false)
            case DirectedEdge(edge, EdgeDirection.N2F) => updateDeterministicMaxN2F(edge)
          }
        }
        for (n <- fg.nodes if !n.variable.isObserved) n.variable.fixMapSetting()
      }
    }

    for(n <- fg.nodes if !n.variable.isObserved) n.variable.setToArgmax()
  }





  def calculateExpectations(fg: FactorGraph, sum: Boolean) {
    fg.expectations = new SparseVector(1000)
    for (factor <- fg.expectationFactors) {
      //update all n2f messages
      for (e <- factor.edges) updateN2F(e)
      if (sum) factor.potential.marginalExpectationsAndObjective(fg.expectations)
      else factor.potential.maxMarginalExpectationsAndObjective(fg.expectations)
    }
  }

  /**
   * Creates a junction tree from the original factor graph, runs BP,
   * then copies value/gradient back into the original factor graph.
   * @param fg the original factor graph
   */

  def onJunctionTree(fg: FactorGraph, bpType: BPType = Max, gradientAndObjective: Boolean = true, forceJunctionTree: Boolean = false) {
    if (!forceJunctionTree && !fg.isLoopy) {
      LoggerUtil.once(LoggerUtil.warn, "Junction tree belief propagation called on a non-loopy graph. Ignoring.")
      apply(fg, 1, schedule = Auto, bpType, gradientAndObjective)
    } else {
      val jt = Junkify(fg)
      apply(jt, 1, schedule = Auto, bpType, gradientAndObjective)
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

  def powF2N(f:Factor, sum: Boolean): Unit = {
    f.edges.foreach(_.msgs.saveCurrentF2NAsOld())
    if (sum) f.potential.powMarginalF2N() else f.potential.powMaxMarginalF2N()
  }

  /**
   * Updates the message from a node to a factor.
   * @param edge the factor-node edge.
   */
  def updateN2F(edge: Edge) {
    edge.n.variable.updateN2F(edge)
  }

  def powN2F(n:Node): Unit = {
    n.variable.powN2F()
  }

  /**
   * Updates the deterministic max-product message from a node to a factor.
   * @param edge the factor-node edge.
   */
  def updateDeterministicMaxN2F(edge: Edge) {
    edge.n.variable.fixMapSetting()
    edge.n.variable.deterministicN2F(edge)
  }

  /**
   * Updates the belief (sum of incoming messages) at a node.
   * @param node the node to update.
   */
  def updateBelief(node: Node, sum: Boolean) {
    if (sum) node.variable.updateMarginalBelief() else node.variable.updateMaxMarginalBelief()
  }

  private def converged(fg: FactorGraph, threshold:Double): Boolean = {
    val residual = fg.edges.view.map(_.msgs match {
      case m:DiscreteMsgs => sqDiff(m.f2n, m.f2nLast)
      case m:TupleMsgs => sqDiff(m.f2n.array, m.f2nLast.array)
    }).sum
    residual < threshold
  }

}







