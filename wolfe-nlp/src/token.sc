import ml.wolfe.{FactorGraph, FactorieVector}

trait Var[T] {

  var setting:T

}

trait Potential {
  def contVars:Seq[Var[Double]]
  def discVars:Seq[Var[Int]]
  def valueForSettings(state:State): Double
}

trait LinearPotential extends Potential {

  def statsForSettings():FactorieVector
}


trait Edge[Msg] {
  def content:Msg

}
trait Factor[Msg] {
  def edges:IndexedSeq[Edge[Msg]]
}



trait Factor2[DiscMsg,ContMsg] {
  def contEdges:IndexedSeq[Edge[ContMsg]]
  def discEdges:IndexedSeq[Edge[DiscMsg]]

}

trait DiscOnlyFactor[Msgs] extends Factor2[Msgs,Nothing] {
  def contEdges = IndexedSeq.empty
}


trait BPMsgs {

}

trait BPPotential extends Potential {
  def marginalF2N(factor:Factor2[BPMsgs,BPMsgs], target:Edge)
}

class EdgeContent {
  type Cont
  type Disc
}


trait State {
  def value[T](variable:Var[T]): Option[T]
}

class DiscreteVar[T] extends Var[T]

object BPFG {
  type DiscContent = String
  type ContContent = String

  class Edge2 {
    val content:DiscContent = ???
  }

}

trait BPFG extends FG {
  type DiscContent = String
  def createDiscContent(variable: Any) = "Yo"
  type Edge = BPFG.Edge2
}


trait FG[E <: EdgeContent] {

  type A = E#Cont
  type DiscContent
  type ContContent
  class Edge {
    def content:DiscContent = ???
  }

  def contEdges:Seq[Edge] = ???
  def discEdges:Seq[Edge[DiscContent]] = ???
  def factors:Seq[Factor2[DiscContent,ContContent]] = ???

  def createDiscContent(variable:Any):DiscContent


}

class BPContent extends EdgeContent {
  type Cont = String
  type Disc = String

}
case class Problem(discVars:Seq[Var[Int]], contVars:Seq[Var[Double]], potentials:Seq[Potential])

def buildBPFG(problem:Problem):BPFG = {
  ???
}

def runBP(fg:BPFG): Unit = {

}

val fg = new FG[BPContent] {}
fg.contEdges.head.content.substring(0,2)
