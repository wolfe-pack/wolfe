package scalapplcodefest

/**
 * @author Sebastian Riedel
 */
trait SetValue[T] extends Set[T] {
  def +(elem: T) = SetUtil.Union(Set(this, Set(elem)))
  def -(elem: T) = SetUtil.SetMinus(this, Set(elem))
}

/**
 * A term that is evaluated to a range of integers.
 * @param from starting integer (included)
 * @param to end integer (excluded)
 */
case class RangeSet(from: Term[Int], to: Term[Int]) extends Term[Set[Int]] {
  def eval(state: State) =
    for (f <- from.eval(state);
         t <- to.eval(state)) yield Range(f, t).toSet
  def variables = SetUtil.Union(Set(from.variables, to.variables))
  def default = Range(from.default, to.default).toSet
  def domain[C >: Set[Int]] = Constant(Util.setToBeImplementedLater)
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


