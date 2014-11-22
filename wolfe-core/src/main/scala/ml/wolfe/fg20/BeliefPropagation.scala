package ml.wolfe.fg20

import ml.wolfe.MoreArrayOps._
import ml.wolfe._
import scalaxy.loops._


trait Residuals extends EdgeMsgsFG {

  trait Msgs {
    def saveCurrentAsLast()
    def residual(): Double
  }

}

trait Residuals2 extends EdgeMsgsFG2 {

  trait Msgs {
    def saveCurrentAsLast()
    def residual(): Double
  }

}


trait BeliefPropagationFG extends Residuals with NodeContentFG {

  final class FactorType(val pot: Pot) extends Factor {

    var discSetting: Array[Int]     = null
    var discObs    : Array[Boolean] = null
    var discDims   : Array[Int]     = null

    def iterateDiscSettings(body: Int => Unit): Unit = {
      updateBuffers()
      TablePotential.allSettings(discDims, discObs)(discSetting)(body)
    }

    private[BeliefPropagationFG] var updated = false

    private def updateBuffers(): Unit = {
      if (!updated) {
        discSetting = Array.ofDim[Int](discEdges.length)
        discObs = Array.ofDim[Boolean](discEdges.length)
        discDims = discEdges.map(_.node.variable.dom.size)
        for (i <- 0 until discEdges.length)
          if (discEdges(i).node.observed) {
            discObs(i) = true
            discSetting(i) = discEdges(i).node.setting
          }
        updated = true
      }
    }
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
    val f2n     = Array.ofDim[Double](size)
    val n2f     = Array.ofDim[Double](size)
    val f2nLast = Array.ofDim[Double](size)
    def saveCurrentAsLast(): Unit = {
      set(f2nLast, f2n)
    }
    def residual() = sqDiff(f2n, f2nLast)
  }

  class ContMsgs() extends Msgs {
    var mean    : Double = 0.0
    var dev     : Double = 0.0
    var lastMean: Double = 0.0
    var lastDev : Double = 0.0
    def saveCurrentAsLast() = {
      lastMean = mean
      lastDev = dev
    }
    def residual() = 0.0
  }

  class VectMsgs() extends Msgs {
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
        for (i <- d.msgs.n2f.indices)
          d.msgs.n2f(i) = { for (e <- d.node.edges if e != edge) yield e.msgs.f2n(i) }.sum
      case _ =>
    }
  }

  def updateBelief(node: Node) = {
    node match {
      case d: DiscNode =>
        for (i <- d.variable.dom.indices)
          d.content.belief(i) = { for (e <- d.edges) yield e.msgs.f2n(i) }.sum
      case _ =>
    }
  }


}

trait EdgePropagation extends Residuals with Scheduling {

  lazy val scheduled = MPSchedulerImpl.schedule()

  def propagate: (Int, Double) => FactorieVector => Unit =
    propagateSchedule(scheduled)

  def propagateSchedule(schedule: Seq[DirectedEdge])
                       (maxIterations: Int, eps: Double)
                       (weights: FactorieVector) {
    var iteration = 0
    var converged = false
    while (iteration < maxIterations && !converged) {
      for (directedEdge <- scheduled) directedEdge.direction match {
        case EdgeDirection.N2F =>
          updateN2F(directedEdge.edge)

        case EdgeDirection.F2N =>
          updateF2N(directedEdge.edge, weights)
      }
      iteration += 1
      converged = residual() < eps
    }
  }

  def residual() = {
    edges.map(_.msgs.residual()).sum
  }


  def updateF2N(edge: Edge, weights: FactorieVector)
  def updateN2F(edge: Edge)

}

trait EdgePropagation2 extends Residuals2 with Scheduling2 {

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

  def propagateFwd: (Int, Double) => FactorieVector => Unit =
    propagateSchedule(fwdSchedule)

  def propagateBwd: (Int, Double) => FactorieVector => Unit =
    propagateSchedule(bwdSchedule)

}

trait FwdBwdEdgePropagation2 extends EdgePropagation2 {

  lazy val fwdSchedule = MPSchedulerImpl.scheduleForward()

  lazy val bwdSchedule = fwdSchedule.reverse.map(_.swap)

  override lazy val scheduled = fwdSchedule ++ bwdSchedule

  def propagateFwd: (Int, Double) => Unit =
    propagateSchedule(fwdSchedule)

  def propagateBwd: (Int, Double) => Unit =
    propagateSchedule(bwdSchedule)

}


object MaxProduct {
  trait Potential extends DiscPotential {
    def discMaxMarginalF2N(edge: BeliefPropagationFG#DiscEdge, weights: FactorieVector)
    def contMaxMarginalF2N(edge: BeliefPropagationFG#ContEdge, weights: FactorieVector): Unit = hasNoContVars
    def maxMarginalExpectationsAndObjective(factor: BeliefPropagationFG#FactorType,
                                            dstExpectations: FactorieVector,
                                            weights: FactorieVector): Double
  }

  trait Processor extends fg20.Processor {
    def discMaxMarginalF2N(varIndex: Int, partialSetting: PartialSetting, incoming: Msgs, result: DiscMsg)
    def maxMarginalObjective(partialSetting: PartialSetting, incoming: Msgs): Double
  }

  trait Potential2 extends fg20.Potential2 {type Proc <: Processor }

  trait ExpFamProcessor extends fg20.ExpFamProcessor with Processor {
    def maxMarginalExpectationsAndObjective(partialSetting: PartialSetting,
                                            incoming: Msgs,
                                            dstExpectations: FactorieVector): Double
    def maxMarginalObjective(partialSetting: PartialSetting, incoming: Msgs) = {
      maxMarginalExpectationsAndObjective(partialSetting, incoming, new SparseVector(0))
    }
  }

  trait ExpFamPotential extends fg20.ExpFamPotential with Potential2 {type Proc <: ExpFamProcessor }

}


class MaxProduct(val problem: Problem) extends BeliefPropagationFG with FwdBwdEdgePropagation {

  type Pot = MaxProduct.Potential

  def acceptPotential = { case pot: MaxProduct.Potential => pot }

  private var deterministicRun = false

  override def updateN2F(edge: Edge) = {
    if (deterministicRun) deterministicN2FAndSetting(edge)
    else updateN2FBySum(edge)
  }

  def updateF2N(edge: Edge, weights: FactorieVector) = {
    edge match {
      case d: DiscEdge => edge.factor.pot.discMaxMarginalF2N(d, weights); d.msgs.saveCurrentAsLast()
      case c: ContEdge => edge.factor.pot.contMaxMarginalF2N(c, weights); c.msgs.saveCurrentAsLast()
    }
  }

  def deterministicN2FAndSetting(edge: Edge) = {
    setToArgmax(edge.node)
    edge match {
      case d: DiscEdge =>
        fill(d.msgs.n2f, Double.NegativeInfinity)
        d.msgs.n2f(d.node.setting) = 0
      case _ =>
    }
  }

  def inferMAP(maxIterations: Int = 10, eps: Double = 0.0001, weights: FactorieVector = new DenseVector(0)): MAPResult = {
    deterministicRun = false
    propagate(maxIterations, eps)(weights)
    for (node <- nodes) { updateBelief(node); normalizeBelief(node) }
    val gradient = new SparseVector(1000)
    val score = factors.view.map(f => f.pot.maxMarginalExpectationsAndObjective(f, gradient, weights)).sum
    val maxMarginals = var2DiscNode.values.map(n =>
      DiscBelief(n.variable) -> Distribution.disc(n.variable.dom, n.content.belief.map(math.exp)))

    deterministicRun = true
    clearSettings()
    propagateFwd(1, eps)(weights)
    nodes.filter(!isFixedSetting(_)).foreach(setToArgmax)
    val discState = problem.discVars.map(v => v -> v.dom(var2DiscNode(v).setting)).toMap[Var[Any], Any]
    val contState = problem.contVars.map(v => v -> var2ContNode(v).setting)
    MAPResult(new MapBasedState(discState ++ contState), score, gradient, new MapBasedState(maxMarginals.toMap))
  }

  def normalizeBelief(node: Node) {
    node match {
      case d: DiscNode => maxNormalize(d.content.belief)
    }
  }

  def setToArgmax(n: Node): Unit = n match {
    case d: DiscNode =>
      val scores = for (i <- d.variable.dom.indices) yield
        (for (e <- d.edges) yield e.msgs.f2n(i)).sum
      d.setting = d.variable.dom.indices.maxBy(scores)
  }

  def clearSettings(): Unit = {
    discNodes.foreach(_.setting = Int.MinValue)
    contNodes.foreach(_.setting = Double.MinValue)
    vectNodes.foreach(_.setting = null)
  }

  def isFixedSetting(n: Node) = n match {
    case d: DiscNode => d.setting != Int.MinValue
    case c: ContNode => c.setting != Double.MinValue
    case v: VectNode => v.setting != null
  }

}

object SumProduct {
  trait Potential extends DiscPotential {
    def discMarginalF2N(edge: BeliefPropagationFG#DiscEdge, weights: FactorieVector)
    def contMarginalF2N(edge: BeliefPropagationFG#ContEdge, weights: FactorieVector): Unit = hasNoContVars
    def marginalExpectationsAndObjective(factor: BeliefPropagationFG#FactorType,
                                         dstExpectations: FactorieVector,
                                         weights: FactorieVector): Double
  }

  trait Processor extends fg20.Processor {
    def discMarginalF2N(varIndex: Int, partialSetting: PartialSetting, incoming: Msgs, result: DiscMsg)
    def marginalObjective(partialSetting: PartialSetting, incoming: Msgs): Double
  }

  trait Potential2 extends fg20.Potential2 {type Proc <: Processor }

  trait ExpFamProcessor extends fg20.ExpFamProcessor with Processor {
    def marginalExpectationsAndObjective(partialSetting: PartialSetting,
                                         incoming: Msgs,
                                         dstExpectations: FactorieVector): Double
    def marginalObjective(partialSetting: PartialSetting, incoming: Msgs) = {
      marginalExpectationsAndObjective(partialSetting, incoming, new SparseVector(0))
    }
  }

  trait ExpFamPotential extends fg20.ExpFamPotential with Potential2 {type Proc <: ExpFamProcessor }

}


class SumProduct(val problem: Problem) extends BeliefPropagationFG with EdgePropagation {

  type Pot = SumProduct.Potential

  def acceptPotential = { case pot: SumProduct.Potential => pot }

  def updateN2F(edge: Edge) = updateN2FBySum(edge)

  def updateF2N(edge: Edge, weights: FactorieVector) = {
    edge match {
      case d: DiscEdge => edge.factor.pot.discMarginalF2N(d, weights); d.msgs.saveCurrentAsLast()
      case c: ContEdge => edge.factor.pot.contMarginalF2N(c, weights); c.msgs.saveCurrentAsLast()
    }
  }

  def inferMarginals(maxIterations: Int = 10, eps: Double = 0.0001, weights: FactorieVector = new DenseVector(0)): MarginalResult = {
    propagate(maxIterations, eps)(weights)
    for (node <- nodes) { updateBelief(node); normalizeBelief(node) }
    val gradient = new SparseVector(1000)
    val logZ = factors.view.map(f => f.pot.marginalExpectationsAndObjective(f, gradient, weights)).sum
    val marginals = var2DiscNode.values.map(n =>
      DiscBelief(n.variable) -> Distribution.disc(n.variable.dom, n.content.belief.map(math.exp)))
    val stats = new SparseVector(1000)
    for (f <- statsFactors) f.pot.marginalExpectationsAndObjective(f, stats, new SparseVector(0))
    MarginalResult(logZ, gradient, new MapBasedState(marginals.toMap))
  }

  def normalizeBelief(node: Node): Double = {
    node match {
      case d: DiscNode => normalizeLogProb(d.content.belief)
    }
  }
}

class MaxProduct2(val problem: Problem) extends BeliefPropagationFG2 with FwdBwdEdgePropagation2 {

  type Pot = MaxProduct.Potential2

  def acceptPotential = { case pot: MaxProduct.Potential2 => pot }

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

  def inferMAP(maxIterations: Int = 10, eps: Double = 0.0001): MAPResult = {
    deterministicRun = false
    propagate(maxIterations, eps)
    for (node <- nodes; if !node.observed) { updateBelief(node); normalizeBelief(node) }
    val gradient = new SparseVector(1000)
    val score = factors.iterator.map(f => f.processor match {
      case p: MaxProduct.ExpFamProcessor => p.maxMarginalExpectationsAndObjective(f.partialSetting, f.incoming, gradient)
      case p: MaxProduct.Processor => p.maxMarginalObjective(f.partialSetting, f.incoming)
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


trait BeliefPropagationFG2 extends Residuals2 with NodeContentFG2 {

  type Pot <: Potential2

  final class FactorType(val pot: Pot) extends Factor {

    var partialSetting: PartialSetting = null // new PartialSetting()

    private[BeliefPropagationFG2] var updated = false

    def updateBuffers(): Unit = {
      if (!updated) {
        partialSetting = new PartialSetting(discEdges.length, contEdges.length, vectEdges.length)
        for (i <- 0 until discEdges.length)
          if (discEdges(i).node.observed) {
            partialSetting.discObs(i) = true
            partialSetting.disc(i) = discEdges(i).node.setting
          }
        for (i <- 0 until contEdges.length) {
          if (contEdges(i).node.observed) {
            partialSetting.contObs(i) = true
            partialSetting.cont(i) = contEdges(i).node.setting
          }
        }
        for (i <- 0 until vectEdges.length) {
          if (vectEdges(i).node.observed) {
            partialSetting.vectObs(i) = true
            partialSetting.vect(i) = vectEdges(i).node.setting
          }
        }
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

class SumProduct2(val problem: Problem) extends BeliefPropagationFG2 with EdgePropagation2 {

  type Pot = SumProduct.Potential2

  def entropy(discNode:DiscNode) = {
    var result = 0.0
    for (i <- (0 until discNode.content.belief.length).optimized) {
      result -= math.exp(discNode.content.belief(i)) * discNode.content.belief(i) //messages are in log space
    }
    result
  }

  def acceptPotential = { case pot: SumProduct.Potential2 => pot }

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
      case p: SumProduct.ExpFamProcessor => p.marginalExpectationsAndObjective(f.partialSetting, f.incoming, gradient)
      case p: SumProduct.Processor => p.marginalObjective(f.partialSetting, f.incoming)
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






