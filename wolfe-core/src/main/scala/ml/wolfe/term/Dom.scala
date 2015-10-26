package ml.wolfe.term

import ml.wolfe.Tensor

/**
 * A domain Dom represents a set of values that a particular term can evaluate to.
 *
 * @tparam T the type of values contained in the domain.
 * @author riedel
 */
trait Dom[+T]

/**
 * The domain of all sequences over a certain domain, with certain limits on length.
 * @param elemDom the domain of the sequence elements.
 * @param minLength the minimum length of the sequences.
 * @param maxLength the maximum length of the sequences.
 * @tparam E the type of elements.
 */
case class SeqDom[+E](elemDom: Dom[E], minLength: Int, maxLength: Int) extends Dom[Seq[E]]

case class RangeDom(range: Range) extends Dom[Int]

case object Ints extends Dom[Int]

case object Doubles extends Dom[Double]

case class Tuple2Dom[+T1, +T2](dom1: Dom[T1], dom2: Dom[T2]) extends Dom[(T1, T2)]

case class ProductDom[+T <: Product](doms: Seq[Dom[Any]], constructor: Seq[Any] => T) extends Dom[T]

case class TensorDom(dims:List[Int]) extends Dom[Tensor] {
  override def toString = "R^{" + dims.mkString(" x ") + "}"
}