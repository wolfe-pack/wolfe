package ml.wolfe.term

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import Traversal._
import Transformer._

/**
 * @author riedel
 */
class MaxProductBP(val objRaw: DoubleTerm,
                   val wrt: Seq[Var[Dom]],
                   val observed: Settings,
                   val msgs: Msgs) extends Argmaxer2 {

  val observedVars = objRaw.vars.filterNot(wrt.contains)
  val result: Settings = Settings.fromSeq(wrt.map(_.domain.createSetting()))

  //get sum terms from objective
  val obj = (clean _ andThen groundSums andThen flattenSums)(objRaw)

  //factor graph
  val fg = new FG[NodeContent, EdgeContent, FactorContent]

  //evaluators for lengths of variable length sums
  val lengthEvals = new ArrayBuffer[Evaluator2]()

  addNodes(obj.vars)
  addTerm(obj)

  class NodeContent()

  class EdgeContent(val maxMarginalizer: MaxMarginalizer2, val f2n: Msg, n2f: Msg)

  class FactorContent()

  def addNodes(vars: Seq[Variable]): Unit = {
    for (v <- vars)
      fg.addNode(v, new NodeContent())
  }

  def addTerm(term: Term[Dom]): Unit = {
    term match {
      case s@VarSeqSum(_) =>
        val factors = s.elements map addPotential
        val obsVarInLength = s.length.vars.filter(observedVars.contains)
        val obsInLength = observed.linkedSettings(observedVars,obsVarInLength)
        val lengthEval = s.length.evaluatorImpl(obsInLength)
        lengthEvals += lengthEval
        fg.declareDynamic(lengthEval.output.disc(0),factors)
        //todo: this needs to work for nested var seq sums
      case Sum(args) =>
        for (a <- args) addTerm(a)
      case pot =>
        addPotential(pot.asInstanceOf[DoubleTerm])
    }
  }

  def addPotential(pot: DoubleTerm): fg.Factor = {
    val vars = pot.vars
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

  def argmax()(implicit execution: Execution) = {

  }


}

class FG[NodeContent, EdgeContent, FactorContent] {

  class Node(val variable: Variable, val content: NodeContent) {
    val edges = new ArrayBuffer[Edge]
    val activeEdges = new ArrayBuffer[Edge]
  }

  class Edge(val node: Node, val factor: Factor, val content: EdgeContent)

  class Factor(val potential: DoubleTerm, val content: FactorContent) {
    val edges = new ArrayBuffer[Edge]
  }

  abstract class DynamicFactors(val factors: IndexedSeq[Factor]) {
    def currentLength: Int

    def currentActive = factors.take(currentLength)
  }

  val nodes = new mutable.LinkedHashMap[Variable, Node]
  val factors = new mutable.LinkedHashMap[DoubleTerm, Factor]
  val edges = new ArrayBuffer[Edge]()
  val dynamic = new ArrayBuffer[DynamicFactors]()
  val static = new mutable.HashSet[Factor]
  val activeDynamicFactors = new ArrayBuffer[Factor]

  def addNode(variable: Variable, content: NodeContent): Node = {
    val node = new Node(variable, content)
    nodes(variable) = node
    node
  }

  def addFactor(potential: DoubleTerm, content: FactorContent)(edgeContent: Node => EdgeContent): Factor = {
    val factor = new Factor(potential, content)
    for (v <- potential.vars; n <- nodes.get(v)) {
      val edge = new Edge(n, factor, edgeContent(n))
      edges += edge
      factor.edges += edge
      n.edges += edge
      n.activeEdges += edge
    }
    static += factor
    factors(potential) = factor
    factor
  }

  def declareDynamic(dynamicLength: => Int, factors: Seq[Factor]): Unit = {
    dynamic += new DynamicFactors(factors.toIndexedSeq) {
      def currentLength = dynamicLength
    }
    for (f <- factors) {
      static.remove(f)
      for (e <- f.edges) {
        e.node.activeEdges -= e
      }
    }
  }

  def updateActiveFactors(): Unit = {
    //remove current active factors {
    for (f <- activeDynamicFactors) {
      deactiveFactor(f)
    }
    activeDynamicFactors.clear()
    for (d <- dynamic) {
      for (f <- d.currentActive) {
        activeDynamicFactors += f
      }
    }
  }

  private def deactiveFactor(f: Factor): Unit = {
    for (e <- f.edges) {
      e.node.activeEdges -= e
    }
  }

  private def activeFactor(f: Factor): Unit = {
    for (e <- f.edges) {
      e.node.activeEdges += e
    }
  }
}