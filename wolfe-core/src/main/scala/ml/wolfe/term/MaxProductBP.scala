package ml.wolfe.term

import ml.wolfe.term.Transformer._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

case class MaxProductParameters(iterations: Int)

/**
 * @author riedel
 */
class MaxProductBP(val objRaw: DoubleTerm,
                   val wrt: Seq[AnyVar],
                   val observed: Settings,
                   val msgs: Msgs)(implicit params: MaxProductParameters) extends Argmaxer {

  import params._

  val observedVars = objRaw.vars.filterNot(wrt.contains)
  val result: Settings = Settings.fromSeq(wrt.map(_.domain.createSetting()))

  //get sum terms from objective
  val obj = (groundSums _ andThen flattenSums andThen clean andThen groundVariables(wrt))(objRaw)

  //factor graph
  val fg = new FG[NodeContent, EdgeContent, FactorContent]

  //all hidden atoms
  val atoms = wrt flatMap MaxProductBP.allAtoms

  //grounders take potentially ungrounded atoms and create grounded atoms based on the current observations
  val atomGrounders = (obj.vars collect {case a:Atom[Dom] => (a:AnyVar) -> a.grounder(observed.linkedSettings(observedVars, a.varsToGround))}).toMap

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

  class NodeContent(val belief: Msg)

  class EdgeContent(val maxMarginalizer: MaxMarginalizer, val f2n: Msg, val n2f: Msg)

  class FactorContent()

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

  def hidden(variable:AnyVar) = variable match {
    case a:AnyAtom => wrt.contains(a.owner)
    case _ => false
  }


  //todo: this is nasty: somehow vars may be "equal" but have different hash values! This is a hack around this
  private def distinctHack[T](args:Seq[T]):Seq[T] = {
    val result = new ArrayBuffer[T]
    for (a <- args) {
      if (!result.contains(a)) {
        result += a
      }
    }
    result.toSeq

  }

  def addPotential(pot: DoubleTerm): fg.Factor = {
    val vars = pot.vars.filter(hidden)
    val n2fs = vars.map(v => v -> v.domain.createZeroMsg()).toMap
    val obsVarsInPot = pot.vars.filter(observedVars.contains)
    val obsInPot = observed.linkedSettings(observedVars, obsVarsInPot)

    fg.addFactor(pot.asInstanceOf[DoubleTerm], new FactorContent(), vars.contains, targetFor) { variable =>
      val otherVars = vars.filterNot(_ == variable)
      val otherN2Fs = Msgs(otherVars map n2fs)
      val maxMarginalizer = pot.maxMarginalizerImpl(otherVars, obsVarsInPot)(obsInPot, otherN2Fs)
      new EdgeContent(maxMarginalizer, maxMarginalizer.outputMsgs(0), n2fs(variable))
    }
  }

  def updateN2Fs(edge: fg.Edge): Unit = {
    for (e <- edge.factor.edges; if e != edge) {
      e.content.n2f := 0.0
      for (o <- e.node.edges; if o != e) {
        e.content.n2f += o.content.f2n
      }
    }
  }

  def updateF2N(edge: fg.Edge)(implicit execution: Execution): Unit = {
    edge.content.maxMarginalizer.maxMarginals()
  }

  def updateNodeBelief(node: fg.Node): Unit = {
    node.content.belief := 0.0
    for (e <- node.edges) {
      node.content.belief += e.content.f2n
    }
  }

  def integrateAtomIntoResult(node:fg.Node): Unit = {
    val atom = node.variable
    node.content.belief.argmax(result(wrt.indexOf(atom.owner)),atom.offsets)
  }

  def argmax()(implicit execution: Execution) = {
    //clear the current factor graph
    fg.deactivate()

    //reset the mapping of unground to ground atoms
    clearGroundings(execution)

    //add the active factors
    factors.activate()

    //the actual message passing
    for (i <- 0 until iterations) {
      for (e <- fg.activeEdges) {
        updateN2Fs(e)
        updateF2N(e)
      }
    }

    //update beliefs on nodes
    result.foreach(_.resetToZero())
    for (n <- fg.activeNodes) {
      updateNodeBelief(n)
      integrateAtomIntoResult(n)
    }

    //compose atomic results to results for the original

  }

  def clearGroundings(execution: Execution): Unit = {
    currentExecution = execution
    currentGroundings.clear()
  }

  def targetFor(source: AnyVar) = {
    currentGroundings.getOrElseUpdate(source, atomGrounders(source).ground()(currentExecution))
  }


}

object MaxProductBP {

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
}

