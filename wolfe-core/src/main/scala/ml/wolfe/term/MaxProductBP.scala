package ml.wolfe.term

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import Traversal._
import Transformer._

case class MaxProductParameters(iterations:Int)

/**
 * @author riedel
 */
class MaxProductBP(val objRaw: DoubleTerm,
                   val wrt: Seq[Var[Dom]],
                   val observed: Settings,
                   val msgs: Msgs)(implicit params:MaxProductParameters) extends Argmaxer {

  import params._

  val observedVars = objRaw.vars.filterNot(wrt.contains)
  val result: Settings = Settings.fromSeq(wrt.map(_.domain.createSetting()))

  //get sum terms from objective
  val obj = (clean _ andThen groundSums andThen flattenSums)(objRaw)

  //factor graph
  val fg = new FG[NodeContent, EdgeContent, FactorContent]

  addNodes(wrt)
  val factors = addTerm(obj)

  sealed trait FactorGroup {
    def activate()(implicit execution: Execution)
  }
  case class SingleFactor(factor:fg.Factor) extends FactorGroup {
    def activate()(implicit execution: Execution) = {
      fg.activate(factor)
    }
  }

  case class FixedLengthFactors(groups:Seq[FactorGroup]) extends FactorGroup {
    def activate()(implicit execution: Execution) = {
      groups foreach (_.activate())
    }

  }

  case class VariableLengthFactors(groups:Seq[FactorGroup], lengthEval:Evaluator) extends FactorGroup {
    def activate()(implicit execution: Execution) = {
      lengthEval.eval()
      val length = lengthEval.output.disc(0)
      groups.take(length) foreach (_.activate())
    }
  }

  class NodeContent(val belief:Msg)

  class EdgeContent(val maxMarginalizer: MaxMarginalizer, val f2n: Msg, val n2f: Msg)

  class FactorContent()

  def addNodes(vars: Seq[Variable]): Unit = {
    for (v <- vars) {
      fg.addNode(v, new NodeContent(v.domain.createMsg()))
    }
  }

  def addTerm(term: Term[Dom]): FactorGroup = {
    term match {
      case s@VarSeqSum(_) =>
        val factors = s.elements map addTerm
        val obsVarInLength = s.length.vars.filter(observedVars.contains)
        val obsInLength = observed.linkedSettings(observedVars,obsVarInLength)
        val lengthEval = s.length.evaluatorImpl(obsInLength)
        VariableLengthFactors(factors,lengthEval)
      case Sum(args) =>
        FixedLengthFactors(args.map(addTerm))
      case pot =>
        SingleFactor(addPotential(pot.asInstanceOf[DoubleTerm]))
    }
  }

  def addPotential(pot: DoubleTerm): fg.Factor = {
    val vars = pot.vars.filter(wrt.contains)
    val n2fs = vars.map(v => v -> v.domain.createZeroMsg()).toMap
    val obsVarsInPot = vars.filter(observedVars.contains)
    val obsInPot = observed.linkedSettings(observedVars,obsVarsInPot)

    fg.addFactor(pot.asInstanceOf[DoubleTerm], new FactorContent()) { node =>
      val otherVars = vars.filterNot(_ == node.variable)
      val otherN2Fs = Msgs(otherVars map n2fs)
      val maxMarginalizer = pot.maxMarginalizerImpl(otherVars, obsVarsInPot)(obsInPot, otherN2Fs)
      new EdgeContent(maxMarginalizer,maxMarginalizer.outputMsgs(0),n2fs(node.variable))
    }
  }

  def updateN2Fs(edge:fg.Edge): Unit = {
    for (e <- edge.factor.activeEdges; if e != edge) {
      e.content.n2f := Double.NegativeInfinity
      for (o <- e.node.activeEdges; if o != e) {
        e.content.n2f += o.content.f2n
      }
    }
  }

  def updateF2N(edge:fg.Edge)(implicit execution: Execution): Unit = {
    edge.content.maxMarginalizer.maxMarginals()
  }

  def updateNodeBelief(node:fg.Node): Unit = {
    node.content.belief := Double.NegativeInfinity
    for (e <- node.activeEdges) {
      node.content.belief += e.content.f2n
    }
  }

  def argmax()(implicit execution: Execution) = {
    fg.deactivate()
    factors.activate()

    for (i <- 0 until iterations) {
      for (e <- fg.activeEdges) {
        updateN2Fs(e)
        updateF2N(e)
      }
    }

    for (n <- fg.activeNodes) {
      updateNodeBelief(n)
      n.content.belief.argmax(result(wrt.indexOf(n.variable)))
    }

  }


}

