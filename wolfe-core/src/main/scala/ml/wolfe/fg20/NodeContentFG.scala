package ml.wolfe.fg20

import ml.wolfe.FactorieVector

import scala.reflect.ClassTag


case class MAPResult(state: State, score: Double, gradient: FactorieVector, maxMarginals: State)
case class MarginalResult(logZ: Double, gradient: FactorieVector, marginals: State)


trait NodeContentFG extends FG {
  type NodeContent
  type DiscNodeContent <: NodeContent
  type ContNodeContent <: NodeContent
  trait NodeType extends Node {
    def content: NodeContent
  }
  final class DiscNode(val variable: DiscVar[Any], val content: DiscNodeContent) extends BasicDiscNode with NodeType
  final class ContNode(val variable: ContVar, val content: ContNodeContent) extends BasicContNode with NodeType

  def createDiscNodeContent(variable: DiscVar[Any]): DiscNodeContent
  def createContNodeContent(contVar: ContVar): ContNodeContent
  def createDiscNode(v: DiscVar[Any]) = new DiscNode(v, createDiscNodeContent(v))
  def createContNode(v: ContVar) = new ContNode(v, createContNodeContent(v))
}

trait EdgeMsgsFG extends FG {
  type Msgs
  type DiscMsgs <: Msgs
  type ContMsgs <: Msgs

  def createDiscMsgs(variable: DiscVar[Any]): DiscMsgs
  def createContMsgs(variable: ContVar): ContMsgs

  def discEdgeTag = reflect.classTag[DiscEdge]
  def contEdgeTag = reflect.classTag[ContEdge]


  trait EdgeType extends Edge {
    def msgs: Msgs
  }

  abstract class TypedEdgeWithMsgs[N <: Node, M <: Msgs] extends TypedEdge[N] with EdgeType {
    def msgs: M
  }

  final class DiscEdge(val node: DiscNode,
                       val factor: Factor,
                       val index: Int,
                       val msgs: DiscMsgs) extends TypedEdgeWithMsgs[DiscNode, DiscMsgs]

  final class ContEdge(val node: ContNode,
                       val factor: Factor,
                       val index: Int,
                       val msgs: ContMsgs) extends TypedEdgeWithMsgs[ContNode, ContMsgs]

  def createDiscEdge(n: DiscNode, f: Factor, index: Int) = new DiscEdge(n, f, index, createDiscMsgs(n.variable))
  def createContEdge(n: ContNode, f: Factor, index: Int) = new ContEdge(n, f, index, createContMsgs(n.variable))
}

trait EmptyEdgeFG extends FG {
  trait EdgeType extends Edge

  final class DiscEdge(val node: DiscNode,
                       val factor: Factor,
                       val index: Int) extends TypedEdge[DiscNode] with EdgeType

  final class ContEdge(val node: ContNode,
                       val factor: Factor,
                       val index: Int) extends TypedEdge[ContNode] with EdgeType

  def createDiscEdge(n: DiscNode, f: Factor, index: Int) = new DiscEdge(n, f, index)
  def createContEdge(n: ContNode, f: Factor, index: Int) = new ContEdge(n, f, index)

  def discEdgeTag = reflect.classTag[DiscEdge]
  def contEdgeTag = reflect.classTag[ContEdge]

}

trait EmptyFactorFG extends FG {
  final class FactorType(val pot: Pot) extends Factor
  def createFactor(pot: Pot) = new FactorType(pot)
}
