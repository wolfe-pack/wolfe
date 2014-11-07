package ml.wolfe.fg20

import ml.wolfe._

// ----------- Factor Graph ---------------


class MyImplies(premise: DiscVar[Boolean], consequent: DiscVar[Boolean]) extends BP.Pot {

  def discVars = Array(premise, consequent)
  def valueInBPFG(factor: BPFG#Factor) = {
    val p = factor.discEdges(0)
    val c = factor.discEdges(1)
    if (!premise.dom(p.node.content.setting) || consequent.dom(c.node.content.setting)) 0.0 else Double.NegativeInfinity
  }
  def maxMarginalExpectationsAndObjective(factor: BPFG#Factor, dstExpectations: FactorieVector) = ???
  def discMaxMarginalF2N(edge: BPFG#DiscEdge) = ???
}


object BP {
  trait Pot extends DiscPotential {
    def valueInBPFG(factor: BPFG#Factor): Double
    def discMaxMarginalF2N(edge: BPFG#DiscEdge)
    def maxMarginalExpectationsAndObjective(factor:BPFG#Factor,dstExpectations: FactorieVector): Double
  }
}


class BPFG(val problem: Problem) extends FG {
  class DiscNodeContent(var setting: Int = 0,
                        var belief: Array[Double])
  class ContNodeContent(var setting:Double = 0.0,
                        var mean: Double = 0.0,
                        var dev: Double = 0.0)
  class FactorContent()
  class DiscMsgs(size: Int) {
    val f2n = Array.ofDim[Double](size)
    val n2f = Array.ofDim[Double](size)
  }
  class ContMsgs()

  type Pot = BP.Pot
  def checkPot(potential: Potential) = potential match {
    case pot: BP.Pot => pot
    case _ => sys.error("BP Requires BP potentials")
  }


  def acceptPotential = {case pot: BP.Pot => pot}
  def createDiscMsgs(variable: DiscVar[Any]) = new DiscMsgs(variable.dom.size)
  def createDiscNodeContent(variable: DiscVar[Any]) = new DiscNodeContent(0, Array.ofDim[Double](variable.dom.size))
  def createContNodeContent(contVar: ContVar) = new ContNodeContent()
  def createContMsgs(contVar: ContVar) = new ContMsgs()

  def createFactorContent(pot: Pot) = new FactorContent

}


// ------------- Inference ---------------
object MaxProduct {

  type Forward = Boolean

  def apply(fg: BPFG): MAPResult = {
    def schedule: Seq[(BPFG#DiscEdge, Forward)] = ???

    for ((edge, forward) <- schedule) forward match {
      case true  =>
        updateDiscN2F(edge)

      case false =>
        edge.factor.pot.discMaxMarginalF2N(edge)
    }



    //...

    val gradient = new SparseVector(???)
    val score = fg.factors.view.map(f => f.pot.maxMarginalExpectationsAndObjective(f,gradient)).sum
    val discState = fg.problem.discVars.map(v => v -> v.dom(fg.discNodes(v).content.setting)).toMap[Var[Any],Any]
    val contState = fg.problem.contVars.map(v => v -> fg.contNodes(v).content.setting)
    MAPResult(new State(discState ++ contState),score,gradient)
  }

  def updateDiscN2F(edge: BPFG#DiscEdge) = {
    for (i <- edge.msgs.n2f.indices)
      edge.msgs.n2f(i) = { for (e <- edge.node.edges if e != edge) yield e.msgs.f2n(i) }.sum
  }
}

