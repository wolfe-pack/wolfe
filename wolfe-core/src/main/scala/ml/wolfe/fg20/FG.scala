package ml.wolfe.fg20

import ml.wolfe.fg.Junkify.Edge

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag

/**
 * A factor graph is a bipartite graph with two types of vertices: nodes and factors.
 * This implementation is tailored to represent sums of potentials over a set of variables. Here each node represents one
 * variable, and each factor represents a potential.
 *
 * The FG class further supports different types of nodes: discrete, continuous, vector-based etc.
 *
 * FGs can be further customized to have specialized implementations of nodes, edges and factors. For example,
 * a FG subclass can use edges to store messages from nodes to factor.
 */
trait FG {
  type Pot <: Potential

  type FactorType <: Factor
  type EdgeType <: Edge
  type NodeType <: Node

  type DiscEdge <: TypedEdge[DiscNode] with EdgeType
  type ContEdge <: TypedEdge[ContNode] with EdgeType
  type DiscNode <: BasicDiscNode with NodeType
  type ContNode <: BasicContNode with NodeType

  implicit def discEdgeTag: ClassTag[DiscEdge]
  implicit def contEdgeTag: ClassTag[ContEdge]

  trait BasicDiscNode extends TypedNode[DiscVar[Any], DiscEdge] {
    var setting  = 0
    var observed = false

    def observe(setting:Int,observed:Boolean = true): Unit = {
      this.setting = setting
      this.observed = observed
    }

  }

  trait BasicContNode extends TypedNode[ContVar, ContEdge]  {
    var setting  = 0.0
    var observed = false
  }


  trait Node {
    def variable: Var[Any]
    def edgeList: List[EdgeType]
  }

  abstract class TypedNode[V <: Var[Any], E <: EdgeType : ClassTag] extends Node {
    def variable: V
    def edgeList = edges.toList
    var edges: Array[E] = null

    private[FG] def build() { edges = buffer.toArray }
    private[FG] var buffer: List[E] = Nil
  }

  trait Edge {
    def node: NodeType
    def factor: FactorType
    def index: Int
  }

  trait TypedEdge[N <: NodeType] extends Edge {
    def node: N
  }

  abstract class Factor {
    def pot: Pot
    var discEdges: Array[DiscEdge] = null
    var contEdges: Array[ContEdge] = null
    def edges = discEdges.iterator ++ contEdges.iterator

  }

  def problem: Problem

  def createDiscNode(v: DiscVar[Any]): DiscNode
  def createContNode(v: ContVar): ContNode
  def createDiscEdge(n: DiscNode, f: FactorType, index: Int): DiscEdge
  def createContEdge(n: ContNode, f: FactorType, index: Int): ContEdge
  def createFactor(pot: Pot): FactorType

  def acceptPotential: PartialFunction[Potential, Pot]

  private def checkPot(p: Potential): Pot =
    if (acceptPotential.isDefinedAt(p)) acceptPotential(p) else sys.error("Unsupported potential")

  def createAndLinkFactor(p: Pot) = {
    val factor = createFactor(p)
    createDiscEdges(factor)
    createContEdges(factor)
    factor
  }

  def nodes = discNodes.values.iterator ++ contNodes.values.iterator
  def edges = factors.iterator.flatMap(_.edges)

  val discNodes = createDiscNodes()
  val contNodes = createContNodes()

  val factors = problem.pots.map(p => createAndLinkFactor(checkPot(p)))

  discNodes.values.foreach(_.build())
  contNodes.values.foreach(_.build())

  setObservations()

  def setObservations() {
    for (v <- problem.observation.domain) {
      v match {
        case d: DiscVar[_] =>
          discNodes(d).observe(d.dom.indexOf(problem.observation(d)))
      }
    }
  }

  //---- These are hacks to make Intellij be able to parse this file, there are much nicer ways to do this otherwise


  private def createDiscEdges(factor: FactorType): Unit = {
    val result = new ArrayBuffer[DiscEdge]
    for ((v, i) <- factor.pot.discVars.zipWithIndex) result += createDiscEdge(discNodes(v), factor, i)
    factor.discEdges = result.toArray[DiscEdge](discEdgeTag)
    factor.discEdges.foreach(e => e.node.buffer ::= e)
  }

  private def createContEdges(factor: FactorType): Unit = {
    val result = new ArrayBuffer[ContEdge]
    for ((v, i) <- factor.pot.contVars.zipWithIndex) result += createContEdge(contNodes(v), factor, i)
    factor.contEdges = result.toArray[ContEdge](contEdgeTag)
    factor.contEdges.foreach(e => e.node.buffer ::= e)
  }


  private def createDiscNodes() = {
    val map = new mutable.HashMap[DiscVar[Any], DiscNode]
    for (v <- problem.discVars) map(v) = createDiscNode(v)
    Map() ++ map
  }

  private def createContNodes() = {
    val map = new mutable.HashMap[ContVar, ContNode]
    for (v <- problem.contVars) map(v) = createContNode(v)
    Map() ++ map
  }


}