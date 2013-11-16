package scalapplcodefest

/**
 * A linear model: dot product of feature and weights, added to a base measure.
 * @author Sebastian Riedel
 */
case class LinearModel(features:Term[Vector],weights:Variable[Vector],base:Term[Double] = Constant(0.0))
  extends Term[Double] with ProxyTerm[Double] {
  import TermImplicits._
  def self = (features dot weights) + base
}
