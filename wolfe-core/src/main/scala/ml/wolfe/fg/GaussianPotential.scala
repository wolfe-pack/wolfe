package ml.wolfe.fg

import ml.wolfe.FactorGraph.Edge
import ml.wolfe.Wolfe.logDist._

/**
 * Created by luke on 24/08/14.
 */
final class GaussianPotential(val meanEdge:Edge, val devEdge:Edge)(val xEdge:Edge) extends Potential {
  val meanVar:ContinuousVar = meanEdge.n.variable.asContinuous
  val devVar:ContinuousVar = devEdge.n.variable.asContinuous
  val xVar:ContinuousVar = xEdge.n.variable.asContinuous

  override def valueForCurrentSetting(): Double = {
    math.log(gaussian(meanVar.value, devVar.value)(xVar.value))
  }

  def proposeMean() = if(! meanVar.isObserved) meanVar.setting = sampleGaussian(xVar.value, devVar.value)
  def proposeDev()  = if(! devVar.isObserved)  ???
  def proposeX()    = if(! xVar.isObserved)    xVar.setting = sampleGaussian(meanVar.value, devVar.value)

  override def proposeSetting(edge: Edge): Unit = edge match {
    case `meanEdge` => proposeMean()
    case `devEdge` => proposeDev()
    case `xEdge` => proposeX()
  }
}
