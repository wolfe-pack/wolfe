package ml.wolfe.fg20

import ml.wolfe.fg.Junkify.Edge

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag

trait BasicFG {
  type Pot <: Potential

  type FactorType <: Factor
  type EdgeType <: Edge
  type NodeType <: Node

  type DiscEdge <: TypedEdge[DiscNode] with EdgeType
  type ContEdge <: TypedEdge[ContNode] with EdgeType
  type DiscNode <: TypedNode[DiscVar[Any],DiscEdge] with NodeType
  type ContNode <: TypedNode[ContVar,ContEdge] with NodeType

  implicit def discEdgeTag: ClassTag[DiscEdge]
  implicit def contEdgeTag: ClassTag[ContEdge]

  trait Node {
    def variable: Var[Any]
    def edgeList: List[EdgeType]
  }

  abstract class TypedNode[V <: Var[Any], E <: EdgeType : ClassTag] extends Node {
    def variable:V
    def edgeList = edges.toList
    private[BasicFG] var buffer: List[E] = Nil
    var edges: Array[E] = null
    private[BasicFG] def build() { edges = buffer.toArray }
  }

  trait Edge {
    def node: Node
    def factor: Factor
    def index: Int
  }

  trait TypedEdge[N <: Node] extends Edge {
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
  def createDiscEdge(n:DiscNode, f:Factor, index:Int): DiscEdge
  def createContEdge(n:ContNode, f:Factor, index:Int): ContEdge
  def createFactor(pot:Pot):FactorType

  def acceptPotential: PartialFunction[Potential, Pot]

  private def checkPot(p: Potential): Pot =
    if (acceptPotential.isDefinedAt(p)) acceptPotential(p) else sys.error("Unsupported potential")

  def createAndLinkFactor(p:Pot) = {
    val factor = createFactor(p)
    createDiscEdges(factor)
    createContEdges(factor)
    factor
  }

  val discNodes = createDiscNodes()
  val contNodes = createContNodes()

  val factors   = problem.pots.map(p => createAndLinkFactor(checkPot(p)))
  def edges     = factors.iterator.flatMap(_.edges)
//  val discEdges = factors.flatMap(_.discEdges)

  discNodes.values.foreach(_.build())
  contNodes.values.foreach(_.build())

  //---- These are hacks to make Intellij be able to parse this file, there are much nicer ways to do this otherwise

  private def createDiscEdges(factor:FactorType): Unit = {
    val result = new ArrayBuffer[DiscEdge]
    for ((v,i) <- factor.pot.discVars.zipWithIndex) result += createDiscEdge(discNodes(v),factor, i)
    factor.discEdges = result.toArray[DiscEdge](discEdgeTag)
    factor.discEdges.foreach(e => e.node.buffer ::= e)
  }

  private def createContEdges(factor:FactorType): Unit = {
    val result = new ArrayBuffer[ContEdge]
    for ((v,i) <- factor.pot.contVars.zipWithIndex) result += createContEdge(contNodes(v),factor, i)
    factor.contEdges = result.toArray[ContEdge](contEdgeTag)
    factor.contEdges.foreach(e => e.node.buffer ::= e)
  }


  private def createDiscNodes() = {
    val map = new mutable.HashMap[DiscVar[Any],DiscNode]
    for (v <- problem.discVars) map(v) = createDiscNode(v)
    Map() ++ map
  }

  private def createContNodes() = {
    val map = new mutable.HashMap[ContVar,ContNode]
    for (v <- problem.contVars) map(v) = createContNode(v)
    Map() ++ map
  }


}