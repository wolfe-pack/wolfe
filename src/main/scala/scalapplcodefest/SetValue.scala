package scalapplcodefest

/**
 * @author Sebastian Riedel
 */
trait SetValue[T] extends Set[T] {
  def +(elem: T) = SetUtil.SetUnion(List(this, Set(elem)))
  def -(elem: T) = SetUtil.SetMinus(this, Set(elem))
}


case class FirstOrderOperator[T,R](op:FunTerm[(T,T),R],arguments:Term[Set[T]]) extends Term[R] {
  def eval(state: State) = ???
  def variables = ???
  def domain[C >: R] = ???
  def default = ???
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

