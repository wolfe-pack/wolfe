package scalapplcodefest.value

import scala.language.existentials
import cc.factorie.la.ScalarTensor
import scala.collection.SetProxy
import scalapplcodefest._
import scalapplcodefest.term.{State, Constant, Term, FunTerm}

/**
 * A set that performs lazy set union and set minus
 * @author Sebastian Riedel
 */
trait SetValue[T] extends Set[T] {
  def +(elem: T): Set[T] = SetUtil.SetUnion(List(this, Set(elem)))
  def -(elem: T): Set[T] = SetUtil.SetMinus(this, Set(elem))
  def --(that: Set[T]): Set[T] = SetUtil.SetMinus(this, that)
  def ++(that: Set[T]): Set[T] = SetUtil.SetUnion(List(this, that))
}

/**
 * Helper to build SetValue objects.
 */
case class SeqSet[T](elems: Seq[T]) extends SetValue[T] {
  val self = elems.toSet
  def contains(elem: T) = self(elem)
  def iterator = elems.iterator
}

/**
 * Turns a set with specific type to a GENeric set over Any objects.
 * @param set the set to convert to a generic set.
 * @tparam T type of elements in original set.
 */
case class Gen[T](set: Set[T]) extends SetProxy[Any] {
  def self = set.asInstanceOf[Set[Any]]
}

/**
 * A Term that represents the reduce operation applied to a sequence of values.
 * @param op the binary operator used to reduce elements.
 * @param arguments the elements to be reduced.
 * @tparam T the type of elements to reduce.
 */
case class Reduce[T](op: Term[Fun[(T, T), T]], arguments: Term[Seq[T]]) extends Term[T] {
  val FunTerm(_, funRange) = op
  def eval(state: State) = for (f <- op.eval(state); set <- arguments.eval(state)) yield
    set.reduce((a1, a2) => f(a1 -> a2))
  def variables = SetUtil.SetUnion(List(op.variables, arguments.variables))
  def domain[C >: T] = funRange.asInstanceOf[Term[Set[C]]]
  def default = funRange.default.head
}

/**
 * A term that is evaluated to a range of integers.
 * @param from starting integer (included)
 * @param to end integer (excluded)
 */
case class RangeSet(from: Term[Int], to: Term[Int]) extends Term[Set[Int]] {
  def eval(state: State) =
    for (f <- from.eval(state);
         t <- to.eval(state)) yield RangeSetValue(f, t)
  def variables = SetUtil.SetUnion(List(from.variables, to.variables))
  def default = RangeSetValue(from.default, to.default + 1)
  def domain[C >: Set[Int]] = Constant(new AllOfType[C])
  override def toString = s"($from ~~ $to)"
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

  case object Range extends BinaryOperatorSameDomain[Int,Set[Int]] {
    def dom = Ints
    def funRange = new AllOfType[Set[Int]]
    def apply(x:(Int,Int)) = RangeSetValue(x._1,x._2)
  }
}

trait AllObjectsLarge[T] extends AllObjects[T] {
  def iterator = Util.tooLargeToIterate
  override def toString() = getClass.getSimpleName
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

import scala.reflect._

class AllOfType[T] extends AllObjectsLarge[T] {
  val tag = manifest
  override def equals(that: Any) = that match {
    case a:AllOfType[_] => true  //todo: Waargh
    case _ => false
  }
}

case object All extends AllObjectsLarge[Any]

case object AllRefs extends AllObjectsLarge[AnyRef]



