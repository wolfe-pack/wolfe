package ml.wolfe.fg

import ml.wolfe.FactorGraph.Edge
import ml.wolfe.Wolfe.logDist._

/**
 * Created by luke on 24/08/14.
 */
final class GaussianPotential(val meanEdge:Edge, val devEdge:Edge)(val xEdge:Edge) extends Potential {
  val meanVar = meanEdge.n.variable.asTyped[Double]
  val devVar  = devEdge.n.variable.asTyped[Double]
  val xVar    = xEdge.n.variable.asTyped[Double]

  override def valueForCurrentSetting(): Double = {
    gaussian(meanVar.value, devVar.value)(xVar.value)
  }

  def proposeMean() = if(!meanVar.isObserved) meanVar match {
    case v: ContinuousVar  => v.setting = sampleGaussian(xVar.value, devVar.value)
    case v: DiscreteVar[Double] =>
      val cumul = v.domain.scanLeft(0d)((acc, d) => acc + math.exp(gaussian(d, devVar.value)(xVar.value))).tail.zipWithIndex
      val rand = math.random * cumul.last._1
      v.setting = cumul.find(x => x._1 > rand).get._2
    //case _ => throw new CantProposeException
  }
  def proposeX()    = if(!xVar.isObserved) xVar match {
    case v: ContinuousVar => if(!v.isObserved) v.setting = sampleGaussian(meanVar.value, devVar.value)
    case v: DiscreteVar[Double] =>
      val cumul = v.domain.scanLeft(0d)((acc, d) => acc + math.exp(gaussian(meanVar.value, devVar.value)(d))).tail.zipWithIndex
      val rand = math.random * cumul.last._1
      v.setting = cumul.find(x => x._1 > rand).get._2
    //case _ => throw new CantProposeException
  }
  def proposeDev()  = ??? //if(! devVar.isObserved) throw new CantProposeException

  override def proposeSetting(edge: Edge): Unit = edge match {
    case `meanEdge` => proposeMean()
    case `devEdge` => proposeDev()
    case `xEdge` => proposeX()
  }
}
