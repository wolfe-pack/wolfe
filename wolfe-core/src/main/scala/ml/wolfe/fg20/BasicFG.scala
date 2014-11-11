package ml.wolfe.fg20

import scala.reflect.ClassTag

trait BasicFG {
  type Pot <: Potential

  type DiscEdge <: TypedEdge[DiscNode]
  type ContEdge <: TypedEdge[ContNode]
  type DiscNode <: TypedNode[DiscVar[Any],DiscEdge]
  type ContNode <: TypedNode[ContVar,ContEdge]
  type FactorType <: Factor

  implicit def discEdgeTag: ClassTag[DiscEdge]
  implicit def contEdgeTag: ClassTag[ContEdge]

  trait Node {
    def variable: Var[Any]
    def build()
    def edgeList: List[Edge]
  }

  abstract class TypedNode[V <: Var[Any], E <: Edge : ClassTag] extends Node {
    def variable:V
    def edgeList = edges.toList
    private[BasicFG] var buffer: List[E] = Nil
    var edges: Array[E] = null
    def build() { edges = buffer.toArray }
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
    def edges = discEdges.view ++ contEdges.view
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
    factor.discEdges = (for ((v,i) <- p.discVars.view.zipWithIndex) yield createDiscEdge(discNodes(v),factor,i)).toArray
    factor.contEdges = (for ((v,i) <- p.contVars.view.zipWithIndex) yield createContEdge(contNodes(v),factor,i)).toArray
    factor.discEdges.foreach(e => e.node.buffer ::= e)
    factor.contEdges.foreach(e => e.node.buffer ::= e)

    factor
  }

  val discNodes = problem.discVars.map(v => v -> createDiscNode(v)).toMap
  val contNodes = problem.contVars.map(v => v -> createContNode(v)).toMap
  val factors   = problem.pots.map(p => createAndLinkFactor(checkPot(p)))
  val edges     = factors.flatMap(_.edges)
  val discEdges = factors.flatMap(_.discEdges)

  discNodes.values.foreach(_.build())
  contNodes.values.foreach(_.build())

}