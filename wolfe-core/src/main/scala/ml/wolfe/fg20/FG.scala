package ml.wolfe.fg20

import ml.wolfe.FactorieVector

import scala.reflect.ClassTag


class State(map: Map[Var[Any], Any]) {
  def apply[T](v: Var[T]): T = map(v).asInstanceOf[T]
}
case class MAPResult(state: State, score: Double, gradient: FactorieVector)

trait FG {
  type DiscNodeContent
  type ContNodeContent
  type FactorContent
  type DiscMsgs
  type ContMsgs
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


  abstract class Node[V <: Var[Any], E <: Edge[_, _] : ClassTag, NodeContent] {
    var edges: Array[E] = null
    def content: NodeContent
    def variable: V
    var buffer:List[E] = Nil
    def build() { edges = buffer.toArray}
  }
  trait Edge[N <: Node[_, _,_], Msgs] {
    def node: N
    def factor: Factor
    def msgs: Msgs
  }

  class DiscNode(val variable:DiscVar[Any], val content: DiscNodeContent) extends Node[DiscVar[Any], DiscEdge, DiscNodeContent]
  class ContNode(val variable:ContVar, val content: ContNodeContent) extends Node[ContVar, ContEdge, ContNodeContent]

  class DiscEdge(val node: DiscNode, val msgs: DiscMsgs,val factor:Factor) extends Edge[DiscNode,DiscMsgs]
  class ContEdge(val node: ContNode, val msgs: ContMsgs,val factor:Factor) extends Edge[ContNode,ContMsgs]

  class Factor(val pot:Pot, val content:FactorContent) {
    var discEdges: Array[DiscEdge] = null
    var contEdges: Array[ContEdge] = null
  }

  def createContMsgs(contVar: ContVar): ContMsgs

  def createFactor(pot: Pot) = {
    val factor = new Factor(pot, createFactorContent(pot))

    def addDiscEdge(node: DiscNode) = {
      val msgs = createDiscMsgs(node.variable)
      val edge = new DiscEdge(node, msgs,factor)
      node.buffer = edge :: node.buffer
      edge
    }
    def addContEdge(node: ContNode) = {
      val msgs = createContMsgs(node.variable)
      val edge = new ContEdge(node, msgs,factor)
      node.buffer = edge :: node.buffer
      edge
    }

    factor.discEdges = pot.discVars.map(v => addDiscEdge(discNodes(v)))
    factor.contEdges = pot.contVars.map(v => addContEdge(contNodes(v)))
    factor
  }

  def checkPot(potential: Potential): Pot

  val factors   = problem.pots.map(p => createFactor(checkPot(p)))
  val discEdges = factors.flatMap(_.discEdges)
  discNodes.foreach(x => x._2.build())
  contNodes.foreach(x => x._2.build())


}
