package scalapplcodefest

/**
 * @author Sebastian Riedel
 */
trait SetValue[T] extends Set[T] {
  def +(elem: T) = SetUtil.SetUnion(List(this, Set(elem)))
  def -(elem: T) = SetUtil.SetMinus(this, Set(elem))
}


case class Reduce[T](op:FunTerm[(T,T),T],arguments:Term[Set[T]]) extends Term[T] {
  def eval(state: State) = for (f <- op.eval(state); set <- arguments.eval(state)) yield set.reduce((a1,a2) => f(a1->a2))
  def variables = op.variables ++ arguments.variables
  def domain[C >: T] = op.superDomain.asInstanceOf[Term[Set[C]]]
  def default = op.targetSet.default.head
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
  def default = RangeSetValue(from.default, to.default)
  def domain[C >: Set[Int]] = Constant(Util.setToBeImplementedLater)
}

/**
 * Set-version of a [[scala.collection.immutable.Range]].
 * @param from start (included)
 * @param to end (excluded)
 */
case class RangeSetValue(from:Int,to:Int) extends SetValue[Int] {
  val range = Range(from,to)
  val rangeSet = range.toSet
  def contains(elem: Int) = rangeSet(elem)
  def iterator = range.iterator
}

trait AllObjects[T] extends SetValue[T] {
  def contains(elem: T) = true
}

/**
 * Set of all integers.
 */
case object Ints extends AllObjects[Int] {
  def iterator = Util.tooLargeToIterate
  override def size = Util.tooLargeToCount
  override def head = 0
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
case object Strings extends AllObjects[String] {
  def iterator = Util.tooLargeToIterate
  override def head = ""
}

/**
 * All Double objects.
 */
case object Doubles extends AllObjects[Double] {
  def iterator = Util.tooLargeToIterate
  override def head = 0.0
}

class All[T] extends AllObjects[T] {
  def iterator = Util.tooLargeToIterate
}

