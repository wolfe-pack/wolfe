package ml.wolfe

import ml.wolfe.FactorGraph.{EdgeDirection, DirectedEdge, Node, Factor}
import ml.wolfe.fg.AD3GenericPotential
import scala.collection.mutable.ArrayBuffer
// import scalaxy.loops._

/**
 * Dual decomposition with projected subgradient, AD3, ..
 *
 * @author luke
 */
object DualDecomposition {

  /** Alternating Directions Dual Decomposition **/
  def ad3(fg:FactorGraph, maxIteration: Int, initialStepSize: Double = 0.1, parallelize: Boolean = true):Unit = {
    apply(fg, maxIteration, initialStepSize, parallelize, ad3 = true)
  }

  /**
   * Run dual decomposition on a factor graph and sets the MAP assignment to the belief variable of each node in the
   * graph
   * @param fg The message passing factor graph
   * @param maxIteration The maximum number of iterations after which the algorithm gives up
   * @param initialStepSize The step size of the initial iteration.
   * @param parallelize A flag that indicates that each factor's inference can be run in parallel. Defaults to true
   */



  def apply(fg: FactorGraph, maxIteration: Int, initialStepSize:Double = 1,
            parallelize: Boolean = true, ad3:Boolean = false):Unit = {

    val factors = if (parallelize) fg.factors.par else fg.factors

    if(ad3) { factors.foreach(_.potential.ad3Init()) }

    fg.converged = false
    initializeN2FAndBeliefs(fg)
    val convergenceThreshold = 1e-6 * fg.nodes.map(n => n.variable.asDiscrete.dim * n.edges.length).sum
    val previousBeliefs: Array[Array[Double]] = fg.nodes.map(_.variable.asDiscrete.b.clone()).toArray
    var stepSize = initialStepSize

    fg.visualizationMessages = ArrayBuffer[Iterable[(DirectedEdge, Seq[Double])]]()

    for (iter <- 0 until maxIteration if !fg.converged) {

      for(f <- factors) if(ad3) f.potential.quadraticProgramF2N(stepSize, 10) else f.potential.mapF2N()
      fg.addMessagesToVisualization(fg.edges, EdgeDirection.F2N)

      for(i <- fg.nodes.indices) {
        Array.copy(fg.nodes(i).variable.asDiscrete.b, 0, previousBeliefs(i), 0, previousBeliefs(i).length)
        fg.nodes(i).variable.updateAverageBelief()
      }
      if(ad3) stepSize = updateAD3StepSize(stepSize, dualResidual(fg, previousBeliefs), primalResidual(fg))
         else stepSize = initialStepSize / math.sqrt(iter + 1)

      for(n <- fg.nodes; e <- n.edges) n.variable.updateDualN2F(e, stepSize)
      fg.addMessagesToVisualization(fg.edges, EdgeDirection.N2F)

      fg.converged = hasConverged(fg, previousBeliefs, convergenceThreshold)
    }

    for(n <- fg.nodes) n.variable.setToArgmax()

    fg.gradient = new SparseVector(1000)
    Wolfe.FactorGraphBuffer.set(fg)
  }






  private def initializeN2FAndBeliefs(fg: FactorGraph) : Unit = {
    for (factor <- fg.factors;
         edge <- factor.edges) {
      val m = edge.msgs.asDiscrete
      for (i <- 0 until m.n2f.size)
        m.n2f(i) = 0

      val v = edge.n.variable.asDiscrete
      val p = 1d / v.b.size
      for (i <- 0 until v.b.size)
        v.b(i) = p
    }
  }

  private def ad3DualObjective(fg:FactorGraph, stepSize:Double) =
    fg.factors.map(_.potential.asInstanceOf[AD3GenericPotential].dualObjective(stepSize)).sum

  private def updateAD3StepSize(stepSize:Double, dualResidual:Double, primalResidual:Double) = {
    if(primalResidual > dualResidual * 10) stepSize * 2
    else if(dualResidual > primalResidual * 10) stepSize / 2
    else stepSize
  }

  private def primalResidual(fg: FactorGraph) = {
    var r:Double = 0
    for(i <- (0 until fg.nodes.length)) {
      for (e <- fg.nodes(i).edges)
        r += MoreArrayOps.sqDiff(e.msgs.asDiscrete.f2n, fg.nodes(i).variable.asDiscrete.b)
    }
    r
  }

  private def dualResidual(fg: FactorGraph, previousBeliefs: Array[Array[Double]]) = {
    var r:Double = 0
    for(i <- (0 until fg.nodes.length))
      r += fg.nodes(i).edges.length * MoreArrayOps.sqDiff(previousBeliefs(i), fg.nodes(i).variable.asDiscrete.b)
    r
  }

  private def hasConverged(fg: FactorGraph, previousBeliefs:Array[Array[Double]], threshold:Double): Boolean =
    dualResidual(fg, previousBeliefs) < threshold && primalResidual(fg) < threshold
}