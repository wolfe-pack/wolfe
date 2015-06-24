package ml.wolfe.term

import java.util

import ml.wolfe.term.Transformer._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

case class BPParameters(iterations: Int,
                        schedule: BP.Schedule.Schedule = BP.Schedule.default,
                        cachePotentials: Boolean = false)

/**
 * @author riedel
 */
class MaxProductBP(val objRaw: DoubleTerm,
                   val wrt: Seq[AnyVar],
                   val observed: Settings,
                   val msgs: Msgs)(implicit val params: BPParameters) extends BP with Argmaxer {

  def createMsgCalculator(pot: DoubleTerm, vars: Seq[AnyVar], observed: Seq[AnyVar], obsInPot: Settings, msgs: Msgs, reverseMsg: Boolean): MessageCalculator = {
    pot.maxMarginalizerImpl(vars, observed)(obsInPot, msgs, reverseMsg)
  }


}

class SumProductBP(val objRaw: DoubleTerm,
                   val wrt: Seq[AnyVar],
                   val observed: Settings,
                   val msgs: Msgs)(implicit val params: BPParameters) extends BP with Marginalizer with Argmaxer {


  def createMsgCalculator(pot: DoubleTerm, vars: Seq[AnyVar], observed: Seq[AnyVar], obsInPot: Settings, msgs: Msgs, reverseMsg: Boolean): MessageCalculator = {
    pot.marginalizerImpl(vars, observed)(obsInPot, msgs, reverseMsg)
  }


  def updateMessages()(implicit execution: Execution) = {
    doMessagePassing()

    for (n <- fg.activeNodes) {
      integrateAtomIntoResultMsgs(n)
    }
  }

  def integrateAtomIntoResultMsgs(node: fg.Node): Unit = {
    val atom = node.variable
    val ownerIndex = wrt.indexOf(atom.owner) //todo: this is slow, do this in advance
    for (i <- 0 until node.content.belief.disc.length)
      outputMsgs(ownerIndex).disc(atom.offsets.discOff + i).msg = node.content.belief.disc(i).msg //todo: cont variables
  }


  val input = observed
  val outputMsgs = Msgs(wrt map (_.domain.createZeroMsg()))
  val inputMsgs = msgs

}


object BP {

  def allAtoms(variable: AnyVar): List[GroundAtom[Dom]] = allAtoms(VarAtom(variable))

  //todo: make this tail-recursive
  def allAtoms(parent: GroundAtom[Dom]): List[GroundAtom[Dom]] = {
    parent.domain match {
      case s: AnySeqDom =>
        LengthGroundAtom(parent.asInstanceOf[GroundAtom[AnySeqDom]]) ::
          (Range(0, s.maxLength).toList flatMap (i =>
            allAtoms(SeqGroundAtom[Dom, AnySeqDom](parent.asInstanceOf[GroundAtom[AnySeqDom]], i))))
      case d: ProductDom =>
        Nil
      case _ => parent :: Nil
    }
  }

  object Schedule extends Enumeration {
    type Schedule = Value
    val synchronized, default = Value
  }

}

/**
 * @author riedel
 */
trait BP {

  //todo: here we assume that the variables to max/sum over (wrt) are the ones we need to calculate messages for (target)

  import BP._
  import Schedule.Schedule

  def wrt: Seq[AnyVar]

  def objRaw: DoubleTerm

  def params: BPParameters

  def observed: Settings

  val schedule: Schedule = params.schedule

  val observedVars = objRaw.vars.filterNot(wrt.contains)
  val result: Settings = Settings.fromSeq(wrt.map(_.domain.createSetting()))

  val pipeline =
    clean _ andThen
      FeatureTransformer.aggregateFeatures andThen
      groundSums andThen
      flattenSums andThen
      clean andThen
      atomizeVariables(wrt) andThen
      shatterAtoms
  //get sum terms from objective
  val obj = pipeline(objRaw)

  //factor graph
  val fg = new FG[NodeContent, EdgeContent, FactorContent]

  //all hidden atoms
  val atoms = wrt flatMap BP.allAtoms

  //grounders take potentially ungrounded atoms and create grounded atoms based on the current observations
  val atomGrounders = (obj.vars collect {
    case a: Atom[Dom] => (a: AnyVar) -> a.grounder(observed.linkedSettings(observedVars, a.varsToGround))
  }).toMap

  //current mapping from unground to ground atoms (e.g. doc(i+1).sentence(j) -> doc(2).sentence(4))
  val currentGroundings = new mutable.HashMap[AnyVar, AnyGroundAtom]

  //execution in last call to argmax
  var currentExecution: Execution = null

  //atoms become the nodes of the factor graph
  addNodes(atoms)

  //create factor groups from objective
  val factors = addTerm(obj)

  sealed trait FactorGroup {
    def activate()(implicit execution: Execution)
  }

  case class SingleFactor(factor: fg.Factor) extends FactorGroup {
    def activate()(implicit execution: Execution) = {
      fg.activate(factor)
      if (params.cachePotentials) factor.potential.asInstanceOf[CachedPotential[DoubleTerm]].clearCache()
    }
  }

  case class FixedLengthFactors(groups: Seq[FactorGroup]) extends FactorGroup {
    def activate()(implicit execution: Execution) = {
      groups foreach (_.activate())
    }

  }

  case class VariableLengthFactors(groups: Seq[FactorGroup], lengthEval: Evaluator) extends FactorGroup {
    def activate()(implicit execution: Execution) = {
      lengthEval.eval()
      val length = lengthEval.output.disc(0)
      groups.take(length) foreach (_.activate())
    }
  }

  class NodeContent(val belief: Msg, val deterministicBelief: Msg, var f2nChanged: Boolean = true, var isDeterministicBeliefSet: Boolean = false)

  class EdgeContent(val maxMarginalizer: MessageCalculator, val f2n: Msg, val n2f: Msg)

  class FactorContent(val messageCalculator: MessageCalculator) {
    var n2fChanged: Boolean = true

    def update()(implicit execution: Execution) = messageCalculator.updateMessages()
  }

  def addNodes(vars: Seq[AnyGroundAtom]): Unit = {
    for (v <- vars) {
      fg.addNode(v, new NodeContent(v.domain.createZeroMsg(), v.domain.createZeroMsg()))
    }
  }

  def addTerm(term: Term[Dom]): FactorGroup = {
    term match {
      case s@VarSeqSum(_, _) =>
        val factors = s.elements map addTerm
        val obsVarInLength = s.length.vars.filter(observedVars.contains)
        val obsInLength = observed.linkedSettings(observedVars, obsVarInLength)
        val lengthEval = s.length.evaluatorImpl(obsInLength)
        VariableLengthFactors(factors, lengthEval)
      case Sum(args) =>
        FixedLengthFactors(args.map(addTerm))
      case pot =>
        SingleFactor(addPotential(pot.asInstanceOf[DoubleTerm]))
    }
  }

  def hidden(variable: AnyVar) = variable match {
    case a: AnyAtom => wrt.contains(a.owner)
    case _ => false
  }


  //todo: this is nasty: somehow vars may be "equal" but have different hash values! This is a hack around this
  private def distinctHack[T](args: Seq[T]): Seq[T] = {
    val result = new ArrayBuffer[T]
    for (a <- args) {
      if (!result.contains(a)) {
        result += a
      }
    }
    result.toSeq

  }

  def createMsgCalculator(pot: DoubleTerm, vars: Seq[AnyVar], observed: Seq[AnyVar],
                          obsInPot: Settings, msgs: Msgs, reverseMsg: Boolean): MessageCalculator

  def addPotential(pot: DoubleTerm): fg.Factor = {
    val vars = pot.vars.filter(hidden)
    val n2fs = vars.map(v => v -> v.domain.createZeroMsg()).toMap
    val obsVarsInPot = pot.vars.filter(observedVars.contains)
    val obsInPot = observed.linkedSettings(observedVars, obsVarsInPot)
    val potToPutOnFactor =
      if (params.cachePotentials) new CachedPotential[DoubleTerm](vars: _*)(pot.asInstanceOf[DoubleTerm])
      else pot.asInstanceOf[DoubleTerm]

    if (schedule == Schedule.synchronized) {
      val potMaxMarginalizer = createMsgCalculator(potToPutOnFactor, vars, obsVarsInPot, obsInPot, Msgs(vars map n2fs), true)
      fg.addFactor(potToPutOnFactor, new FactorContent(potMaxMarginalizer), vars.contains, targetFor) { variable =>
        val outputMsg = potMaxMarginalizer.outputMsgs(vars.indexOf(variable))
        new EdgeContent(null, outputMsg, n2fs(variable))
      }
    } else {
      fg.addFactor(potToPutOnFactor, new FactorContent(null), vars.contains, targetFor) { variable =>
        val otherVars = vars.filterNot(_ == variable)
        val otherN2Fs = Msgs(otherVars map n2fs)
        val maxMarginalizer = createMsgCalculator(potToPutOnFactor, otherVars, obsVarsInPot, obsInPot, otherN2Fs, false)
        new EdgeContent(maxMarginalizer, maxMarginalizer.outputMsgs(0), n2fs(variable))
      }
    }
  }

  /*def updateN2Fs(edge: fg.Edge): Unit = {
    for (e <- edge.factor.edges; if e != edge) {
      e.content.n2f := 0.0
      for (o <- e.node.edges; if o != e) {
        e.content.n2f += o.content.f2n
      }
      fg.recordN2F(e, e.content.n2f)
    }
  }*/

  def updateN2F(edge: fg.Edge): Unit = {
    edge.content.n2f := 0.0
    for (o <- edge.node.edges; if o != edge) {
      edge.content.n2f += o.content.f2n
    }
    fg.recordN2F(edge, edge.content.n2f)
  }

  def updateDeterministicN2F(edge: fg.Edge): Unit = {
    updateDeterministicBelief(edge.node)
    edge.content.n2f := edge.node.content.deterministicBelief - edge.content.f2n
  }

  def updateF2N(edge: fg.Edge)(implicit execution: Execution): Unit = {
    edge.content.maxMarginalizer.updateMessages()
    edge.node.content.f2nChanged = true
    fg.recordF2N(edge, edge.content.f2n)
  }

  def updateFactor(factor: fg.Factor)(implicit execution: Execution): Unit = {
    for (e <- factor.edges) {
      if (e.node.content.f2nChanged) {
        updateNodeBelief(e.node)
        e.content.n2f := e.node.content.belief - e.content.f2n
        fg.recordN2F(e, e.content.n2f)
        factor.content.n2fChanged = true
      }
    }
    if (factor.content.n2fChanged) {
      factor.content.update()
      for (e <- factor.edges) {
        fg.recordF2N(e, e.content.f2n)
        e.node.content.f2nChanged = true
      }
    }
  }

  def updateFactorDeterministic(factor: fg.Factor)(implicit execution: Execution): Unit = {
    for (e <- factor.edges) {
      if (e.node.content.f2nChanged) {
        updateDeterministicBelief(e.node)
        e.content.n2f := e.node.content.deterministicBelief - e.content.f2n
        factor.content.n2fChanged = true
      }
    }
    if (factor.content.n2fChanged) {
      factor.content.update()
      for (e <- factor.edges) {
        e.node.content.f2nChanged = true
      }
    }
  }

  def updateNodeBelief(node: fg.Node): Unit = {
    if (!node.content.f2nChanged) return
    node.content.belief := 0.0
    for (e <- node.edges) {
      node.content.belief += e.content.f2n
    }
    node.content.f2nChanged = false
  }

  def updateDeterministicBelief(node: fg.Node): Unit = {
    if(! node.content.isDeterministicBeliefSet) {
      node.content.deterministicBelief := 0.0
      for (e <- node.edges) {
        node.content.deterministicBelief += e.content.f2n
      }

      for (discMsg <- node.content.deterministicBelief.disc) {
        val argmax = discMsg.argmax()
        val maxval = discMsg.msg(argmax)
        util.Arrays.fill(discMsg.msg, Double.NegativeInfinity)
        discMsg.msg(argmax) = maxval
      }

      node.content.isDeterministicBeliefSet = true
    }
  }

  def integrateAtomIntoResultState(node: fg.Node): Unit = {
    val atom = node.variable
    node.content.deterministicBelief.argmax(result(wrt.indexOf(atom.owner)), atom.offsets)
  }

  def doMessagePassing()(implicit execution: Execution) = {
    //clear the current factor graph
    fg.deactivate()

    //reset the mapping of unground to ground atoms
    clearGroundings(execution)

    //add the active factors
    factors.activate()

    //the actual message passing
    schedule match {
      case Schedule.synchronized =>
        for (i <- 0 until params.iterations; f <- fg.activeFactors) {
          updateFactor(f)
        }
      case Schedule.default =>
        val mpSchedule = fg.scheduleInwardOutward()
        for (i <- 0 until params.iterations; de <- mpSchedule) {
          if (de.isF2N)
            updateF2N(de.edge)
          else
            updateN2F(de.edge)
        }
    }

    for (n <- fg.activeNodes) {
      updateNodeBelief(n)
    }

    fg.activeNodes.foreach{ _.content.isDeterministicBeliefSet = false }

    schedule match {
      case Schedule.synchronized =>
        for (f <- fg.activeFactors) {
          updateFactorDeterministic(f)
        }
      case Schedule.default =>
        val deterministicSchedule = fg.scheduleOutward()
        for (de <- deterministicSchedule) {
          if (de.isF2N) {
            updateF2N(de.edge)
          } else {
            updateDeterministicN2F(de.edge)
          }
        }
    }

    for (n <- fg.activeNodes) {
      updateDeterministicBelief(n)
    }

//    for((v, n) <- fg.nodes) {
//      println(v)
//      println("  Belief: " + n.content.belief.disc(0).msg.mkString(", "))
//      println("  Determ: " + n.content.deterministicBelief.disc(0).msg.mkString(", "))
//    }
  }

  def argmax()(implicit execution: Execution) = {
    doMessagePassing()

    //update beliefs on nodes
    result.foreach(_.resetToZero())
    for (n <- fg.activeNodes) {
      integrateAtomIntoResultState(n)
    }
  }


  def clearGroundings(execution: Execution): Unit = {
    currentExecution = execution
    currentGroundings.clear()
  }

  def targetFor(source: AnyVar) = {
    currentGroundings.getOrElseUpdate(source, atomGrounders(source).ground()(currentExecution))
  }


}


