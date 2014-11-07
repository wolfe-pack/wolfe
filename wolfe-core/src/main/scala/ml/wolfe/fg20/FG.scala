package ml.wolfe.fg20

import ml.wolfe.FactorieVector



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





  class Factor(val pot: Pot, val content: FactorContent) {
    var discEdges: Array[DiscEdge] = null
    var contEdges: Array[ContEdge] = null
  }

  class Edge[N<:Node[_],Msgs](val node: N, val factor: Factor)
//  trait DiscEdgeTrait {
//    def msgs:DiscMsgs
//    def node:DiscNode
//    def factor:Factor
//  }
  class DiscEdge(node:DiscNode,factor:Factor,val msgs:DiscMsgs) extends Edge(node,factor)
  class ContEdge(node:ContNode,factor:Factor,val msgs:ContMsgs) extends Edge(node,factor)

//  class Node[VarType <: Var[_], E<:Edge[_]](val variable: VarType) {
//    var edgesBuffer: List[E]  = Nil
//    var edges      : Array[E] = null
//
//    def build(): Unit = {
//      edges = edgesBuffer.toArray
//    }
//  }

  class Node[V <: Var[_]](val variable:V) {
//    var edgesBuffer: List[E]  = Nil
//    var edges      : Array[E] = null
  }
  
//  class NodeNeighbors[E <: Edge[_]](){
//    var edgesBuffer: List[E]  = Nil
//    var edges      : Array[E] = null
//  }

  class DiscNode(variable:DiscVar[Any], val content:DiscNodeContent) extends Node(variable) {
    var edgesBuffer: List[DiscEdge]  = Nil
    var edges      : Array[DiscEdge] = null
  }

  class ContNode(variable:ContVar, val content:ContNodeContent) extends Node(variable) {
    var edgesBuffer: List[ContEdge]  = Nil
    var edges      : Array[ContEdge] = null
  }


  def createContMsgs(contVar: ContVar):ContMsgs

  def createFactor(pot: Pot) = {
    val factor = new Factor(pot, createFactorContent(pot))

    def addDiscEdge(node: DiscNode) = {
      val msgs = createDiscMsgs(node.variable)
      val edge = new DiscEdge(node, factor, msgs)
      node.edgesBuffer = edge :: node.edgesBuffer
      edge
    }
    def addContEdge(node: ContNode) = {
      val msgs = createContMsgs(node.variable)
      val edge = new ContEdge(node, factor, msgs)
      node.edgesBuffer = edge :: node.edgesBuffer
      edge
    }

    factor.discEdges = pot.discVars.map(v => addDiscEdge(discNodes(v)))
    factor.contEdges = pot.contVars.map(v => addContEdge(contNodes(v)))
    factor
  }

  def checkPot(potential: Potential): Pot

  val factors   = problem.pots.map(p => createFactor(checkPot(p)))
  val discEdges = factors.flatMap(_.discEdges)
  discNodes.foreach(x => x._2.edges = x._2.edgesBuffer.toArray)
  contNodes.foreach(x => x._2.edges = x._2.edgesBuffer.toArray)


}
