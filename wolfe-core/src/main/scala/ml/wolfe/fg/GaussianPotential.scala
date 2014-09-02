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
    math.log(gaussian(meanVar.value, devVar.value)(xVar.value))
  }

  def proposeMean() = if(!meanVar.isObserved) meanVar match {
    case v: ContinuousVar  => v.setting = sampleGaussian(xVar.value, devVar.value)
    case _ => ???
  }
  def proposeX()    = if(!xVar.isObserved) xVar match {
    case v: ContinuousVar => if(!v.isObserved) v.setting = sampleGaussian(meanVar.value, devVar.value)
    case _ => ???
  }
  def proposeDev()  = if(! devVar.isObserved)  ???

  override def proposeSetting(edge: Edge): Unit = edge match {
    case `meanEdge` => proposeMean()
    case `devEdge` => proposeDev()
    case `xEdge` => proposeX()
  }
}
