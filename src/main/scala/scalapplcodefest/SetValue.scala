package scalapplcodefest

import scala.language.existentials
import cc.factorie.la.ScalarTensor

/**
 * @author Sebastian Riedel
 */
trait SetValue[T] extends Set[T] {
  def +(elem: T): Set[T] = SetUtil.SetUnion(List(this, Set(elem)))
  def -(elem: T): Set[T] = SetUtil.SetMinus(this, Set(elem))
  def --(that: Set[T]): Set[T] = SetUtil.SetMinus(this, that)
  def ++(that: Set[T]): Set[T] = SetUtil.SetUnion(List(this, that))
}

/**
 * A Term that represents the reduce operation applied to a sequence of values.
 * @param op the binary operator used to reduce elements.
 * @param arguments the elements to be reduced.
 * @tparam T the type of elements to reduce.
 */
case class Reduce[T](op: FunTerm[(T, T), T], arguments: Term[Seq[T]]) extends Term[T] {
  def eval(state: State) = for (f <- op.eval(state).right; set <- arguments.eval(state).right) yield
    set.reduce((a1, a2) => f(a1 -> a2))
  def variables = op.variables ++ arguments.variables
  def domain[C >: T] = op.funRange.asInstanceOf[Term[Set[C]]]
  def default = op.funRange.default.head
}

/**
 * Helpers to create compositional objects that correspond to set based operators.
 */
object Quantified {

  trait AbstractQuantified[T] {
    def operator: FunTerm[(T, T), T]
    def apply[A](term: Term[Seq[T]]) = Reduce(operator, term)
    def unapply(term: Term[T]) = term match {
      case Reduce(operator, seq) => Option(seq)
      case _ => None
    }
  }

  object Exists extends AbstractQuantified[Boolean] {def operator = Logic.Or.Term}
  object Forall extends AbstractQuantified[Boolean] {def operator = Logic.And.Term}
  object VecSum extends AbstractQuantified[Vector] {def operator = Math.VecAdd.Term}
  object DoubleSum extends AbstractQuantified[Double] {def operator = Math.DoubleAdd.Term}

}

/**
 * A term that is evaluated to a range of integers.
 * @param from starting integer (included)
 * @param to end integer (excluded)
 */
case class RangeSet(from: Term[Int], to: Term[Int]) extends Term[Set[Int]] {
  def eval(state: State) =
    for (f <- from.eval(state).right;
         t <- to.eval(state).right) yield RangeSetValue(f, t)
  def variables = SetUtil.SetUnion(List(from.variables, to.variables))
  def default = RangeSetValue(from.default, to.default)
  def domain[C >: Set[Int]] = Constant(Util.setToBeImplementedLater)

}

/**
 * Set-version of a [[scala.collection.immutable.Range]].
 * @param from start (included)
 * @param to end (excluded)
 */
case class RangeSetValue(from: Int, to: Int) extends SetValue[Int] {
  val range = Range(from, to)
  val rangeSet = range.toSet
  def contains(elem: Int) = rangeSet(elem)
  def iterator = range.iterator
}

trait AllObjects[T] extends SetValue[T] {
  def contains(elem: T) = true
  override def hashCode() = System.identityHashCode(this)
  override def equals(that: Any) = that match {
    case x: AnyRef => x eq this
    case _ => false
  }
  override def +(elem: T) = this


}

/**
 * Set of all integers.
 */
case object Ints extends AllObjects[Int] {
  def iterator = Util.tooLargeToIterate
  override def size = Util.tooLargeToCount
  override def head = 0
}

trait AllObjectsLarge[T] extends AllObjects[T] {
  def iterator = Util.tooLargeToIterate
}

/**
 * Set of all vectors.
 */
case object Vecs extends AllObjectsLarge[Vec] {
  override def size = Util.tooLargeToCount
  override def head = Vec.zero
}

/**
 * Set of all vectors.
 */
case object Vectors extends AllObjectsLarge[Vector] {
  override def size = Util.tooLargeToCount
  override def head = new ScalarTensor(0.0)
}


/**
 * All Boolean objects.
 */
case object Bools extends AllObjects[Boolean] {
  def iterator = Iterator(false, true)
  override def head = false
}

/**
 * All String objects.
 */
case object Strings extends AllObjectsLarge[String] {
  override def head = ""
}

/**
 * All Double objects.
 */
case object Doubles extends AllObjectsLarge[Double] {
  override def head = 0.0
}

class AllOfType[T] extends AllObjectsLarge[T]

case object All extends AllObjectsLarge[Any]

case object AllRefs extends AllObjectsLarge[AnyRef]



