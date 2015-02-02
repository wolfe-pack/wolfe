package ml.wolfe.fg20

import ml.wolfe.MoreArrayOps._
import ml.wolfe._
import ml.wolfe.term.Setting
import scalaxy.loops._


trait Residuals extends EdgeMsgsFactorGraph {

  trait Msgs {
    def saveCurrentAsLast()
    def residual(): Double
  }

}

trait EdgePropagation extends Residuals with Scheduling {

  lazy val scheduled = MPSchedulerImpl.schedule()

  def propagate: (Int, Double) => Unit =
    propagateSchedule(scheduled)

  def propagateSchedule(schedule: Seq[DirectedEdge])
                       (maxIterations: Int, eps: Double) {
    var iteration = 0
    var converged = false
    while (iteration < maxIterations && !converged) {
      for (directedEdge <- scheduled if !directedEdge.n.observed) directedEdge.direction match {
        case EdgeDirection.N2F =>
          updateN2F(directedEdge.edge)

        case EdgeDirection.F2N =>
          updateF2N(directedEdge.edge)
      }
      iteration += 1
      converged = residual() < eps
    }
  }

  def residual() = {
    edges.map(_.msgs.residual()).sum
  }


  def updateF2N(edge: Edge)
  def updateN2F(edge: Edge)

}



trait FwdBwdEdgePropagation extends EdgePropagation {

  lazy val fwdSchedule = MPSchedulerImpl.scheduleForward()

  lazy val bwdSchedule = fwdSchedule.reverse.map(_.swap)

  override lazy val scheduled = fwdSchedule ++ bwdSchedule

  def propagateFwd: (Int, Double) => Unit =
    propagateSchedule(fwdSchedule)

  def propagateBwd: (Int, Double) => Unit =
    propagateSchedule(bwdSchedule)

}


trait MaxMarginalizer {
  def discMaxMarginalF2N(varIndex: Int, partialSetting: PartialSetting, incoming: Msgs, result: DiscMsg)
  def maxMarginalObjective(partialSetting: PartialSetting, incoming: Msgs): Double
}

trait ExpFamMaxMarginalizer extends MaxMarginalizer {
  def maxMarginalExpectationsAndObjective(partialSetting: PartialSetting,
                                          incoming: Msgs,
                                          dstExpectations: FactorieVector): Double
  def maxMarginalObjective(partialSetting: PartialSetting, incoming: Msgs) = {
    maxMarginalExpectationsAndObjective(partialSetting, incoming, new SparseVector(0))
  }
}

trait SupportsMaxMarginalization extends Potential {
  def maxMarginalizer():MaxMarginalizer
}

trait SupportsExpFamMaxMarginalization extends SupportsMaxMarginalization {
  def maxMarginalizer():ExpFamMaxMarginalizer
}

/**
 * A processor that calculates marginal probabilities with respect to a potential.
 */
trait Marginalizer {
  def discMarginalF2N(varIndex: Int, partialSetting: PartialSetting, incoming: Msgs, result: DiscMsg)
  def marginalObjective(partialSetting: PartialSetting, incoming: Msgs): Double
}

trait ExpFamMarginalizer extends Marginalizer {
  def marginalExpectationsAndObjective(partialSetting: PartialSetting,
                                       incoming: Msgs,
                                       dstExpectations: FactorieVector): Double
  def marginalObjective(partialSetting: PartialSetting, incoming: Msgs) = {
    marginalExpectationsAndObjective(partialSetting, incoming, new SparseVector(0))
  }
}


/**
 * A potential that can spawn marginalizers to calculate marginal probabilities and expectations
 * regardint the potential.
 */
trait SupportsMarginalization extends Potential {
  def marginalizer():Marginalizer
}

/**
 * Potential that can calculate its argmax.
 */
trait SupportsArgmax extends Potential {
  def argmaxer():Argmaxer
}

/**
 * Marginalization for exponential family potentials.
 */
trait SupportsExpFamMarginalization extends SupportsMarginalization with ExpFamPotential {
  def marginalizer():ExpFamMarginalizer
}


class MaxProduct(val problem: Problem[SupportsMaxMarginalization],
                 val maxIterations: Int = 10,
                 val eps: Double = 0.0001) extends BeliefPropagationFactorGraph with FwdBwdEdgePropagation with Argmaxer {

  type Pot = SupportsMaxMarginalization
  type Processor = MaxMarginalizer
  def processor(pot: Pot) = pot.maxMarginalizer()

  private var deterministicRun = false

  override def updateN2F(edge: Edge) = {
    if (deterministicRun) deterministicN2FAndSetting(edge)
    else updateN2FBySum(edge)
  }

  def updateF2N(edge: Edge) = {
    val factor = edge.factor
    edge match {
      case d: DiscEdge =>
        edge.factor.processor.discMaxMarginalF2N(edge.index, factor.partialSetting, factor.incoming, d.msgs.f2n) //pot.discMaxMarginalF2N(d, weights); d.msgs.saveCurrentAsLast()
    }
  }

  def deterministicN2FAndSetting(edge: Edge) = {
    setToArgmax(edge.node)
    edge match {
      case d: DiscEdge =>
        fill(d.msgs.n2f.msg, Double.NegativeInfinity)
        d.msgs.n2f.msg(d.node.setting) = 0
      case _ =>
    }
  }

  def inferMAP(maxIterations: Int = maxIterations, eps: Double = eps): MAPResult = {
    deterministicRun = false
    propagate(maxIterations, eps)
    for (node <- nodes; if !node.observed) { updateBelief(node); normalizeBelief(node) }
    val gradient = new SparseVector(1000)
    val score = factors.iterator.map(f => f.processor match {
      case p: ExpFamMaxMarginalizer => p.maxMarginalExpectationsAndObjective(f.partialSetting, f.incoming, gradient)
      case p: MaxMarginalizer => p.maxMarginalObjective(f.partialSetting, f.incoming)
    }).sum
    val maxMarginals = var2DiscNode.values.map(n =>
      DiscBelief(n.variable) -> Distribution.disc(n.variable.dom, n.content.belief.map(math.exp)))

    deterministicRun = true
    clearSettings()
    propagateFwd(1, eps)
    nodes.filter(n => !n.observed && !isFixedSetting(n)).foreach(setToArgmax)
    val discState = problem.discVars.map(v => v -> v.dom(var2DiscNode(v).setting)).toMap[Var[Any], Any]
    val contState = problem.contVars.map(v => v -> var2ContNode(v).setting)
    MAPResult(new MapBasedState(discState ++ contState), score, gradient, new MapBasedState(maxMarginals.toMap))
  }

  def argmax(observed: PartialSetting, incoming: fg20.Msgs, result: Setting, score: DoubleBuffer): Unit = {

  }


  def normalizeBelief(node: Node) {
    node match {
      case d: DiscNode => maxNormalize(d.content.belief)
    }
  }

  def setToArgmax(n: NodeType): Unit = n match {
    case d: DiscNode =>
      val scores = for (i <- d.variable.dom.indices) yield
        (for (e <- d.edges) yield e.msgs.f2n.msg(i)).sum
      d.setting = d.variable.dom.indices.maxBy(scores)
  }

  def clearSettings(): Unit = {
    discNodes.foreach(_.setting = Int.MinValue)
    contNodes.foreach(_.setting = Double.MinValue)
    vectNodes.foreach(_.setting = null)
  }

  def isFixedSetting(n: NodeType) = n match {
    case d: DiscNode => d.setting != Int.MinValue
    case c: ContNode => c.setting != Double.MinValue
    case v: VectNode => v.setting != null
  }

}


trait BeliefPropagationFactorGraph extends Residuals with NodeContentFactorGraph {

  final class FactorType(val pot: Pot) extends Factor {

    var partialSetting: PartialSetting = null // new PartialSetting()

    private[BeliefPropagationFactorGraph] var updated = false

    def updateBuffers(): Unit = {
      if (!updated) {
        partialSetting = createObservation()
        updated = true
      }
    }

    lazy val incoming = new fg20.Msgs(discEdges.map(_.msgs.n2f), contEdges.map(_.msgs.n2f), vectEdges.map(_.msgs.n2f))

  }

  def createFactor(pot: Pot) = new FactorType(pot)

  type NodeContent = Any

  class DiscNodeContent(var belief: Array[Double])

  class ContNodeContent(var mean: Double = 0.0, // todo: Move into MaxProduct/SumProduct
                        var dev: Double = 0.0)
  // as max-marginals are not a distribution)

  class VectNodeContent(var mean: FactorieVector = null,
                        var dev: FactorieVector = null)


  class DiscMsgs(size: Int) extends Msgs {
    val f2n     = new DiscMsg(size)
    val n2f     = new DiscMsg(size)
    val f2nLast = new DiscMsg(size)
    def saveCurrentAsLast(): Unit = {
      set(f2nLast.msg, f2n.msg)
    }
    def residual() = sqDiff(f2n.msg, f2nLast.msg)
  }

  class ContMsgs() extends Msgs {
    val f2n = new ContMsg
    val n2f = new ContMsg
    def saveCurrentAsLast() = {
    }
    def residual() = 0.0
  }

  class VectMsgs() extends Msgs {
    val f2n = new VectMsg
    val n2f = new VectMsg
    def saveCurrentAsLast() = {}
    def residual() = 0.0
  }


  def createDiscNodeContent(variable: DiscVar[Any]) = new DiscNodeContent(Array.ofDim[Double](variable.dom.size))
  def createContNodeContent(contVar: ContVar) = new ContNodeContent()
  def createVectNodeContent(vectVar: VectVar) = new VectNodeContent()

  def createDiscMsgs(variable: DiscVar[Any]) = new DiscMsgs(variable.dom.size)
  def createContMsgs(contVar: ContVar) = new ContMsgs()
  def createVectMsgs(vectVar: VectVar) = new VectMsgs()

  def updateN2FBySum(edge: Edge) = {
    edge match {
      case d: DiscEdge =>
        for (i <- d.msgs.n2f.msg.indices)
          d.msgs.n2f.msg(i) = { for (e <- d.node.edges if e != edge) yield e.msgs.f2n.msg(i) }.sum
      case _ =>
    }
  }

  def updateBelief(node: Node) = {
    node match {
      case d: DiscNode =>
        for (i <- d.variable.dom.indices)
          d.content.belief(i) = { for (e <- d.edges) yield e.msgs.f2n.msg(i) }.sum
      case _ =>
    }
  }

  for (f <- factors) f.updateBuffers()


}

class SumProduct(val problem: Problem[SupportsMarginalization]) extends BeliefPropagationFactorGraph with EdgePropagation {

  type Pot = SupportsMarginalization
  type Processor = Marginalizer
  def processor(pot: Pot) = pot.marginalizer

  def entropy(discNode:DiscNode) = {
    var result = 0.0
    for (i <- (0 until discNode.content.belief.length).optimized) {
      result -= math.exp(discNode.content.belief(i)) * discNode.content.belief(i) //messages are in log space
    }
    result
  }

  def updateN2F(edge: Edge) = updateN2FBySum(edge)

  def updateF2N(edge: Edge) = {
    val factor = edge.factor
    edge match {
      case d: DiscEdge => edge.factor.processor.discMarginalF2N(edge.index, factor.partialSetting, factor.incoming, d.msgs.f2n); d.msgs.saveCurrentAsLast()
    }
  }

  def inferMarginals(maxIterations: Int = 10, eps: Double = 0.0001): MarginalResult = {
    propagate(maxIterations, eps)
    println(scheduled.map(_.toString))
    for (node <- nodes; if !node.observed) { updateBelief(node); normalizeBelief(node) }
    val gradient = new SparseVector(1000)
    var logZ = factors.iterator.map(f => f.processor match {
      case p: ExpFamMarginalizer => p.marginalExpectationsAndObjective(f.partialSetting, f.incoming, gradient)
      case p: Marginalizer => p.marginalObjective(f.partialSetting, f.incoming)
    }).sum

    //we need to subtract doubly counted node entropies to calculate the bethe objective.
    for (node <- discNodes) {
      logZ += (1.0 - node.edges.size) * entropy(node)
    }

    val marginals = var2DiscNode.values.map(n =>
      DiscBelief(n.variable) -> Distribution.disc(n.variable.dom, n.content.belief.map(math.exp)))
    //val stats = new SparseVector(1000)
    //for (f <- statsFactors) f.pot.marginalExpectationsAndObjective(f, stats, new SparseVector(0))
    MarginalResult(logZ, gradient, new MapBasedState(marginals.toMap))
  }

  def normalizeBelief(node: Node): Double = {
    node match {
      case d: DiscNode => normalizeLogProb(d.content.belief)
    }
  }
}






