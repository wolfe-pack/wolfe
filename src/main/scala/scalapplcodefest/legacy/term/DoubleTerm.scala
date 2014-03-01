package scalapplcodefest.legacy.term

import scalapplcodefest.legacy.value.Doubles

/**
 * @author Sebastian Riedel
 */
trait DoubleTerm extends Term[Double] {
  def default = 0.0
  def domain[C >: Double] = Constant(Doubles).asInstanceOf[Term[Set[C]]]
}
