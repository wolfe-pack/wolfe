package ml.wolfe.fg20

import ml.wolfe.FactorieVector

import scala.reflect.ClassTag


case class ArgmaxResult(state: State, score: Double)
case class MAPResult(state: State, score: Double, gradient: FactorieVector, maxMarginals: State)
case class MarginalResult(logZ: Double, gradient: FactorieVector, marginals: State)


trait NodeContentFactorGraph extends FactorGraph {
  type NodeContent
  type DiscNodeContent <: NodeContent
  type ContNodeContent <: NodeContent
  type VectNodeContent <: NodeContent

  trait NodeType extends Node {
    def content: NodeContent
  }
  final class DiscNode(val variable: DiscVar[Any], val content: DiscNodeContent) extends BasicDiscNode with NodeType
  final class ContNode(val variable: ContVar, val content: ContNodeContent) extends BasicContNode with NodeType
  final class VectNode(val variable: VectVar, val content: VectNodeContent) extends BasicVectNode with NodeType


  def createDiscNodeContent(variable: DiscVar[Any]): DiscNodeContent
  def createContNodeContent(contVar: ContVar): ContNodeContent
  def createVectNodeContent(vectVar: VectVar): VectNodeContent

  def createDiscNode(v: DiscVar[Any]) = new DiscNode(v, createDiscNodeContent(v))
  def createContNode(v: ContVar) = new ContNode(v, createContNodeContent(v))
  def createVectNode(v: VectVar) = new VectNode(v, createVectNodeContent(v))

}



class DiscMsg(dim:Int) {
  var msg = Array.ofDim[Double](dim)
}
class ContMsg {
  var mean:Double = 0.0
}
class VectMsg {
  var mean:FactorieVector = null
}

class Msgs(val disc:Array[DiscMsg],
           val cont:Array[ContMsg],
           val vect:Array[VectMsg])


trait EmptyEdgeFactorGraph extends FactorGraph {
  trait EdgeType extends Edge

  final class DiscEdge(var node: DiscNode,
                       val factor: FactorType,
                       val index: Int) extends TypedEdge[DiscNode] with EdgeType

  final class ContEdge(var node: ContNode,
                       val factor: FactorType,
                       val index: Int) extends TypedEdge[ContNode] with EdgeType

  final class VectEdge(var node: VectNode,
                       val factor: FactorType,
                       val index: Int) extends TypedEdge[VectNode] with EdgeType


  def createDiscEdge(n: DiscNode, f: FactorType, index: Int) = new DiscEdge(n, f, index)
  def createContEdge(n: ContNode, f: FactorType, index: Int) = new ContEdge(n, f, index)
  def createVectEdge(n: VectNode, f: FactorType, index: Int) = new VectEdge(n, f, index)

  def discEdgeTag = reflect.classTag[DiscEdge]
  def contEdgeTag = reflect.classTag[ContEdge]
  def vectEdgeTag = reflect.classTag[VectEdge]

}



trait EmptyFactorFactorGraph extends FactorGraph {
  final class FactorType(val pot: Pot) extends Factor
  def createFactor(pot: Pot) = new FactorType(pot)
}


trait EmptyNodeFactorGraph extends FactorGraph {
  trait NodeType extends Node
  final class DiscNode(val variable: DiscVar[Any]) extends BasicDiscNode with NodeType
  final class ContNode(val variable: ContVar) extends BasicContNode with NodeType
  final class VectNode(val variable: VectVar) extends BasicVectNode with NodeType
  def createDiscNode(v: DiscVar[Any]) = new DiscNode(v)
  def createContNode(v: ContVar) = new ContNode(v)
  def createVectNode(v: VectVar) = new VectNode(v)
}


trait EdgeMsgsFactorGraph extends FactorGraph {
  type Msgs
  type DiscMsgs <: Msgs
  type ContMsgs <: Msgs
  type VectMsgs <: Msgs

  def createDiscMsgs(variable: DiscVar[Any]): DiscMsgs
  def createContMsgs(variable: ContVar): ContMsgs
  def createVectMsgs(variable: VectVar): VectMsgs


  def discEdgeTag = reflect.classTag[DiscEdge]
  def contEdgeTag = reflect.classTag[ContEdge]
  def vectEdgeTag = reflect.classTag[VectEdge]

  trait EdgeType extends Edge {
    def msgs: Msgs
  }

  abstract class TypedEdgeWithMsgs[N <: NodeType, M <: Msgs] extends TypedEdge[N] with EdgeType {
    def msgs: M
  }

  final class DiscEdge(var node: DiscNode,
                       val factor: FactorType,
                       val index: Int,
                       val msgs: DiscMsgs) extends TypedEdgeWithMsgs[DiscNode, DiscMsgs]

  final class ContEdge(var node: ContNode,
                       val factor: FactorType,
                       val index: Int,
                       val msgs: ContMsgs) extends TypedEdgeWithMsgs[ContNode, ContMsgs]

  final class VectEdge(var node: VectNode,
                       val factor: FactorType,
                       val index: Int,
                       val msgs: VectMsgs) extends TypedEdgeWithMsgs[VectNode, VectMsgs]


  def createDiscEdge(n: DiscNode, f: FactorType, index: Int) = new DiscEdge(n, f, index, createDiscMsgs(n.variable))
  def createContEdge(n: ContNode, f: FactorType, index: Int) = new ContEdge(n, f, index, createContMsgs(n.variable))
  def createVectEdge(n: VectNode, f: FactorType, index: Int) = new VectEdge(n, f, index, createVectMsgs(n.variable))
}

