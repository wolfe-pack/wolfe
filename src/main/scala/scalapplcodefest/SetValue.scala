package scalapplcodefest

/**
 * @author Sebastian Riedel
 */
trait SetValue[T] extends Set[T] {
  def +(elem: T) = SetUtil.Union(Set(this,Set(elem)))
  def -(elem: T) = SetUtil.SetMinus(this,Set(elem))
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

/**
 * Set of all integers.
 */
case object Ints extends Term[Set[Int]] {
  val set = new SetValue[Int] {
    def contains(elem: Int) = true
    def iterator = Util.tooLargeToIterate
    override def size = Util.tooLargeToCount
    override def head = 0
  }
  def eval(state: State) = Some(set)
  def variables = Set.empty
  def domain[C >: Set[Int]] = Constant(Util.setToBeImplementedLater)
  def default = set
}

/**
 * All boolean values.
 */
case object Bools extends Term[Set[Boolean]] {
  val set = new SetValue[Boolean] {
    def contains(elem: Boolean) = true
    def iterator = Iterator(false,true)
    override def head = false
  }
  def eval(state: State) = Some(set)
  def variables = Set.empty
  def domain[C >: Set[Boolean]] = Constant(Util.setToBeImplementedLater)
  def default = set
}

