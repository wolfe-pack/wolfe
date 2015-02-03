package ml.wolfe.fg

import ml.wolfe.FactorGraph.Edge

/**
 * Created by luke on 02/09/14.
 */
final class ChoicePotential(x:Edge, xs:Seq[Edge], i:Edge) extends Potential {
  val xVar = x.n.variable.asDiscrete
  val xsVars = xs.map(_.n.variable.asDiscrete)
  val idxVar = i.n.variable.asDiscreteTyped[Int]

  override def valueForCurrentSetting(): Double =
    if(x.n.variable.value == xs(idxVar.setting).n.variable.value) 0 else Double.NegativeInfinity

  override def proposeSetting(edge: Edge): Unit = edge match {
    case `x` => xVar.setting = idxVar.setting
    case `i` => idxVar.setting = xVar.setting
    //case e if xs contains e => throw new CantProposeException
  }
}