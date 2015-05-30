package ml.wolfe.term

import ml.wolfe.term.Transformer._

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

case class MaxProductParameters(iterations: Int)
case class BPParameters(iterations: Int)

/**
 * @author riedel
 */
class MaxProductBP(val objRaw: DoubleTerm,
                   val wrt: Seq[AnyVar],
                   val observed: Settings,
                   val msgs: Msgs)(implicit val params: BPParameters) extends BP with Argmaxer {

  def createMsgCalculator(pot:DoubleTerm, vars:Seq[AnyVar],observed:Seq[AnyVar],obsInPot:Settings,msgs:Msgs, reverseMsg:Boolean):MessageCalculator = {
    pot.maxMarginalizerImpl(vars, observed)(obsInPot, msgs, reverseMsg)
  }

  def argmax()(implicit execution: Execution) = {
    doMessagePassing()

    //update beliefs on nodes
    result.foreach(_.resetToZero())
    for (n <- fg.activeNodes) {
      updateNodeBelief(n)
      integrateAtomIntoResult(n)
    }
  }

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

  import BP._
  import Schedule.Schedule

  def wrt:Seq[AnyVar]
  def objRaw:DoubleTerm
  def params:BPParameters
  def observed:Settings
  
  val schedule: Schedule = Schedule.default

  val observedVars = objRaw.vars.filterNot(wrt.contains)
  val result: Settings = Settings.fromSeq(wrt.map(_.domain.createSetting()))

  //get sum terms from objective
  val obj = (groundSums _ andThen flattenSums andThen clean andThen atomizeVariables(wrt))(objRaw)

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

  class NodeContent(val belief: Msg, var f2nChanged: Boolean = true)

  class EdgeContent(val maxMarginalizer: MessageCalculator, val f2n: Msg, val n2f: Msg)

  class FactorContent(val messageCalculator: MessageCalculator) {
    var n2fChanged: Boolean = true

    def update()(implicit execution: Execution) = messageCalculator.updateMessages()
  }

  def addNodes(vars: Seq[AnyGroundAtom]): Unit = {
    for (v <- vars) {
      fg.addNode(v, new NodeContent(v.domain.createZeroMsg()))
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

  def createMsgCalculator(pot:DoubleTerm, vars:Seq[AnyVar],observed:Seq[AnyVar],
                          obsInPot:Settings,msgs:Msgs, reverseMsg:Boolean):MessageCalculator

  def addPotential(pot: DoubleTerm): fg.Factor = {
    val vars = pot.vars.filter(hidden)
    val n2fs = vars.map(v => v -> v.domain.createZeroMsg()).toMap
    val obsVarsInPot = pot.vars.filter(observedVars.contains)
    val obsInPot = observed.linkedSettings(observedVars, obsVarsInPot)

    if (schedule == Schedule.synchronized) {
      val potMaxMarginalizer = createMsgCalculator(pot, vars, obsVarsInPot, obsInPot, Msgs(vars map n2fs), true)
      fg.addFactor(pot.asInstanceOf[DoubleTerm], new FactorContent(potMaxMarginalizer), vars.contains, targetFor) { variable =>
        val outputMsg = potMaxMarginalizer.outputMsgs(vars.indexOf(variable))
        new EdgeContent(null, outputMsg, n2fs(variable))
      }
    } else {
      fg.addFactor(pot.asInstanceOf[DoubleTerm], new FactorContent(null), vars.contains, targetFor) { variable =>
        val otherVars = vars.filterNot(_ == variable)
        val otherN2Fs = Msgs(otherVars map n2fs)
        val maxMarginalizer = createMsgCalculator(pot, otherVars, obsVarsInPot,obsInPot, otherN2Fs,false)
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

  def updateNodeBelief(node: fg.Node): Unit = {
    if (!node.content.f2nChanged) return
    node.content.belief := 0.0
    for (e <- node.edges) {
      node.content.belief += e.content.f2n
    }
    node.content.f2nChanged = false
  }

  def integrateAtomIntoResult(node: fg.Node): Unit = {
    val atom = node.variable
    node.content.belief.argmax(result(wrt.indexOf(atom.owner)), atom.offsets)
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
        val mpSchedule = fg.scheduleForwardBackward()
        for (i <- 0 until params.iterations; de <- mpSchedule) {
          if (de.isF2N)
            updateF2N(de.edge)
          else
            updateN2F(de.edge)
        }
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


