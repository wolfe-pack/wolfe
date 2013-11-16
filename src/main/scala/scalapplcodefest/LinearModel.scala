package scalapplcodefest

/**
 * A linear model: dot product of feature and weights, added to a base measure.
 * @author Sebastian Riedel
 */
case class LinearModel(features:Term[Vec],weights:Variable[Vec],base:Term[Double]) extends Term[Double] with ProxyTerm[Double] {
  import TermImplicits._
  def self = (features dot weights) + base
}
