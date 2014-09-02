package ml.wolfe.examples

import ml.wolfe.GibbsSampling
import ml.wolfe.Wolfe._
import ml.wolfe.Wolfe.logDist._
import ml.wolfe.macros.OptimizedOperators._

import scala.math._


/**
 * @author Sebastian Riedel
 */
object GaussianMixture extends App {


  case class World(z: Int, x: Double)
  val means   = Seq(-1.0, 0.0, 1.0)
  val mixture = Seq(0.5, 0.25, 0.25)

  implicit def components = 0 until means.size

  @LogZByInference(GibbsSampling(_))
  def model(w: World) = gaussian(means(w.z), 1.0)(w.x) + log(mixture(w.z))

  def worlds = all(World)

  val mu = expect(worlds)(model)(w => oneHot('x, w.x) + oneHot('z, w.z))


}

object FirstOrderGaussianMixture {


  case class World(instances: Seq[Instance])
  case class Instance(z: Int, x: Double)

  val means   = Seq(-1.0, 0.0, 1.0)
  val mixture = Seq(0.5, 0.25, 0.25)

  implicit def components = 0 until means.size
  implicit def instances = seqsOfLength(4, all(Instance))
  def worlds = all(World)

  def model(w: World) = sum(w.instances) { i => gaussian(means(i.z), 1.0)(i.x) + log(mixture(i.z)) }


}

object FirstOrderBayesianGaussianMixture {

  val k = 4
  val n = 5

  case class World(instances: Seq[Instance], phi: Seq[Double], mu: Seq[Double], sig: Seq[Double])
  case class Instance(z: Int, x: Double)

  def worlds = all(World) {
    seqsOfLength(n, all(Instance)) x
    seqsOfLength(n, doubles) x
    seqsOfLength(k, doubles) x
    seqsOfLength(k, doubles)
  }

  def dirichlet(alpha: Double)(categorial: Seq[Double]) = 0.0
  def invGamma(alpha: Double, beta: Double)(x: Double) = 0.0

  val mu_0   = 0.0
  val sig_0  = 0.0
  val lambda = 0.0
  val nu     = 0.0

  def model(w: World) = {
    import w._
    sum(instances) { i => gaussian(mu(i.z), sig(i.z))(i.x) + log(phi(i.z)) } +
    sum(0 until k) { k => dirichlet(0.1)(phi) + invGamma(nu, sig_0)(sig(k)) + gaussian(mu_0, lambda * sig(k))(mu(k)) }
  }


}