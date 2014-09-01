package ml.wolfe.fg

import ml.wolfe.FactorGraph.Edge
import ml.wolfe.Wolfe.logDist._

/**
 * Created by luke on 24/08/14.
 */
final class GaussianPotential(val meanEdge:Edge, val devEdge:Edge)(val xEdge:Edge) extends Potential {
  val meanVar:Var[Double] = meanEdge.n.variable.asTyped[Double]
  val devVar:Var[Double] = devEdge.n.variable.asTyped[Double]
  val xVar:Var[Double] = xEdge.n.variable.asTyped[Double]

  override def valueForCurrentSetting(): Double = {
    math.log(gaussian(meanVar.value, devVar.value)(xVar.value))
  }

  def proposeMean() = meanVar match {
    case v:ContinuousVar => v.setting = sampleGaussian(xVar.value, devVar.value)
    case _ =>
  }

  def proposeDev() = devVar match {
    case v:ContinuousVar => ???
    case _ =>
  }

  def proposeX() = xVar match {
    case v:ContinuousVar => v.setting = sampleGaussian(meanVar.value, devVar.value)
    case _ =>
  }

  override def proposeSetting(edge: Edge): Unit = edge match {
    case `meanEdge` => proposeMean()
    case `devEdge` => proposeDev()
    case `xEdge` => proposeX()
  }
}
