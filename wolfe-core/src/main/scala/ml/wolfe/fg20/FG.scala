//package ml.wolfe.fg20
//
//import ml.wolfe.FactorieVector
//
//
//class Node[VarType <: Var[_], NodeContent, Msgs](val variable:VarType, val content:NodeContent) {
//  var edgesBuffer:List[Edge[Msgs]] = Nil
//  var edges:Array[Edge[Msgs]] = null
//
//  def build(): Unit ={
//    edges = edgesBuffer.toArray
//  }
//}
//
//class Factor[FactorContent](val pot:Potential, val content:FactorContent) {
//  var discEdges: Array[Edge[Msgs]] = null
//  var contEdges: Array[Edge[Msgs]] = null
//  var vectEdges: Array[Edge[Msgs]] = null
//}
//type DiscreteFactor[FactorContent, Msgs] = Factor[FactorContent]
//
//class Edge[Msgs](val node:Node[_, _, Msgs], val factor:Factor[_], val msgs:Msgs)
//
//
////trait FG {
////  type NodeContent
////  type Msgs
////  type FactorContent
////}
//
//class State
//
//case class MAPResult(state: State, score: Double, gradient: FactorieVector)
