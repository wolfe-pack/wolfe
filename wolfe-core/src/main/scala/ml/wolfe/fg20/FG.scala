package ml.wolfe.fg20

import ml.wolfe.FactorieVector

import scala.reflect.ClassTag


case class MAPResult(state: State, score: Double, gradient: FactorieVector, maxMarginals: State)
case class MarginalResult(logZ: Double, gradient: FactorieVector, marginals: State)

trait FG {
  type DiscNodeContent
  type ContNodeContent
  type FactorContent
  type Msgs
  type DiscMsgs <: Msgs
  type ContMsgs <: Msgs
  type Pot <: Potential

  def problem: Problem

  def createDiscMsgs(variable: DiscVar[Any]): DiscMsgs
  def createDiscNodeContent(variable: DiscVar[Any]): DiscNodeContent
  def createContNodeContent(contVar: ContVar): ContNodeContent

  def createFactorContent(pot: Pot): FactorContent

  val discNodes = problem.discVars.map(v =>
    v -> new DiscNode(v, createDiscNodeContent(v))
  ).toMap


  val contNodes = problem.contVars.map(v =>
    v -> new ContNode(v, createContNodeContent(v))
  ).toMap

  val factors   = problem.pots.map(p => createFactor(checkPot(p)))
  val edges     = factors.flatMap(_.edges)
  val discEdges = factors.flatMap(_.discEdges)

  discNodes.values.foreach(_.build())
  contNodes.values.foreach(_.build())

  def nodes: Iterable[Node] = discNodes.values.view ++ contNodes.values.view

  trait Node {
    def variable: Var[Any]
    def edgeList: List[Edge]
  }

  trait Edge {
    def node: Node
    def factor: Factor
    def msgs: Msgs
    def index: Int
  }

  abstract class TypedNode[V <: Var[Any], E <: Edge : ClassTag, NodeContent] extends Node {
    var edges: Array[E] = null
    def edgeList = edges.toList
    def content: NodeContent
    def variable: V
    var buffer: List[E] = Nil
    def build() { edges = buffer.toArray }
  }

  sealed trait TypedEdge[N <: Node, TypedMsgs <: Msgs] extends Edge {
    def node: N
    def factor: Factor
    def msgs: TypedMsgs
  }

  final class DiscNode(val variable: DiscVar[Any], val content: DiscNodeContent) extends TypedNode[DiscVar[Any], DiscEdge, DiscNodeContent]
  final class ContNode(val variable: ContVar, val content: ContNodeContent) extends TypedNode[ContVar, ContEdge, ContNodeContent]

  final class DiscEdge(val node: DiscNode, val factor: Factor, val index: Int, val msgs: DiscMsgs) extends TypedEdge[DiscNode, DiscMsgs]
  final class ContEdge(val node: ContNode, val factor: Factor, val index: Int, val msgs: ContMsgs) extends TypedEdge[ContNode, ContMsgs]

  final class Factor(val pot: Pot, val content: FactorContent) {
    var discEdges: Array[DiscEdge] = null
    var contEdges: Array[ContEdge] = null

    def edges = discEdges.view ++ contEdges.view

  }

  def createContMsgs(contVar: ContVar): ContMsgs

  def createFactor(pot: Pot): Factor = {
    val factor = new Factor(pot, createFactorContent(pot))

    def addDiscEdge(node: DiscNode, index: Int) = {
      val msgs = createDiscMsgs(node.variable)
      val edge = new DiscEdge(node, factor, index, msgs)
      node.buffer = edge :: node.buffer
      edge
    }
    def addContEdge(node: ContNode, index: Int) = {
      val msgs = createContMsgs(node.variable)
      val edge = new ContEdge(node, factor, index, msgs)
      node.buffer = edge :: node.buffer
      edge
    }
    factor.discEdges = pot.discVars.view.zipWithIndex.map(v => addDiscEdge(discNodes(v._1), v._2)).toArray
    factor.contEdges = pot.contVars.view.zipWithIndex.map(v => addContEdge(contNodes(v._1), v._2)).toArray
    factor
  }

  private def checkPot(p: Potential): Pot =
    if (acceptPotential.isDefinedAt(p)) acceptPotential(p) else sys.error("Unsupported potential")

  def acceptPotential: PartialFunction[Potential, Pot]


}

trait NodeContentFG extends BasicFG {
  type NodeContent
  type DiscNodeContent <: NodeContent
  type ContNodeContent <: NodeContent
  trait NodeType extends Node {
    def content:NodeContent
  }
  final class DiscNode(val variable: DiscVar[Any], val content: DiscNodeContent) extends TypedNode[DiscVar[Any], DiscEdge] with NodeType
  final class ContNode(val variable: ContVar, val content: ContNodeContent) extends TypedNode[ContVar, ContEdge] with NodeType

  def createDiscNodeContent(variable: DiscVar[Any]): DiscNodeContent
  def createContNodeContent(contVar: ContVar): ContNodeContent
  def createDiscNode(v: DiscVar[Any]) = new DiscNode(v, createDiscNodeContent(v))
  def createContNode(v: ContVar) = new ContNode(v, createContNodeContent(v))
}

trait EdgeMsgsFG extends BasicFG {
  type Msgs
  type DiscMsgs <: Msgs
  type ContMsgs <: Msgs

  def createDiscMsgs(variable: DiscVar[Any]): DiscMsgs
  def createContMsgs(variable: ContVar): ContMsgs

  trait EdgeType extends Edge {
    def msgs:Msgs
  }

  abstract class TypedEdgeWithMsgs[N <: Node,M <: Msgs] extends TypedEdge[N] with EdgeType {
    def msgs:M
  }

  final class DiscEdge(val node: DiscNode,
                       val factor: Factor,
                       val index: Int,
                       val msgs: DiscMsgs) extends TypedEdgeWithMsgs[DiscNode,DiscMsgs]

  final class ContEdge(val node: ContNode,
                       val factor: Factor,
                       val index: Int,
                       val msgs: ContMsgs) extends TypedEdgeWithMsgs[ContNode,ContMsgs]

  def createDiscEdge(n: DiscNode, f: Factor, index: Int) = new DiscEdge(n, f, index, createDiscMsgs(n.variable))
  def createContEdge(n: ContNode, f: Factor, index: Int) = new ContEdge(n, f, index, createContMsgs(n.variable))
}

trait EmptyFactorFG extends BasicFG {
  final class FactorType(val pot: Pot) extends Factor
  def createFactor(pot: Pot) = new FactorType(pot)
}
