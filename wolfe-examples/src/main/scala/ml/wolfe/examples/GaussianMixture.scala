package ml.wolfe.examples

import ml.wolfe.Wolfe
import Wolfe._
import math._

/**
 * @author Sebastian Riedel
 */
object GaussianMixture extends App {

  import ml.wolfe.macros.OptimizedOperators._

  case class World(z: Int, x: Double)
  val means   = Seq(-1.0, 0.0, 1.0)
  val mixture = Seq(0.5, 0.25, 0.25)

  implicit def components = 0 until means.size

  def model(w: World) = gaussian(means(w.z), 1.0)(w.x) + log(mixture(w.z))

  def worlds = all(World)

  val mu = expect(worlds)(model)(w => oneHot('x -> w.x) + oneHot('z -> w.z))




}

object ComplexGaussianMixture {

  case class Component(mean: Double, variance: Double)
  case class World(instances: Seq[Instance], components: Seq[Component], prior: Seq[Double])
  case class Instance(z: Int, x: Double)


}