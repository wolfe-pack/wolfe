package ml.wolfe.fg20

import ml.wolfe.FactorieVector


class Node[VarType <: Var[_], NodeContent, Msgs](val variable:VarType, val content:NodeContent) {
  var edgesBuffer:List[Edge[Msgs]] = Nil
  var edges:Array[Edge[Msgs]] = null

  def build(): Unit ={
    edges = edgesBuffer.toArray
  }
}

class Factor[FactorContent, DiscMsgs, ContMsgs, VectMsgs](val pot:Potential, val content:FactorContent) {
  var discEdges: Array[Edge[DiscMsgs]] = null
  var contEdges: Array[Edge[ContMsgs]] = null
  var vectEdges: Array[Edge[VectMsgs]] = null

  def value = pot.value(this)
}

class Edge[Msgs](val node:Node[_, _, Msgs], val factor:Factor[_, _, _, _], val msgs:Msgs)////trait FG ////  type NodeConten////  type Msg////  type FactorConten////}

class State(settings:Map[Var, Any])
case class MAPResult(state: State, score: Double, gradient: FactorieVector)
