package scalapplcodefest

import scala.language.existentials

/**
 * @author Sebastian Riedel
 */
trait SetValue[T] extends Set[T] {
  def +(elem: T) = SetUtil.SetUnion(List(this, Set(elem)))
  def -(elem: T) = SetUtil.SetMinus(this, Set(elem))
}

case class Reduce[T](op:FunTerm[(T,T),T],arguments:Term[Seq[T]]) extends Term[T] {
  def eval(state: State) = for (f <- op.eval(state); set <- arguments.eval(state)) yield set.reduce((a1,a2) => f(a1->a2))
  def variables = op.variables ++ arguments.variables
  def domain[C >: T] = op.superDomain.asInstanceOf[Term[Set[C]]]
  def default = op.targetSet.default.head
}

/**
 * Helpers to create compositional objects that correspond to quantification term.
 */
object Quantified {
  def apply[A,B](op:FunTerm[(B,B),B], fun:FunTerm[A,B]) = Reduce(op,ImageSeq(fun,fun.superDomain))
  def unapply[B](term:Term[B]) = term match {
    case Reduce(op,ImageSeq(fun,_)) => Option((op,fun))
    case _ => None
  }

  trait AbstractQuantified[T] {
    def operator:FunTerm[(T,T),T]
    def apply[A](term:FunTerm[A,T]) = Quantified[A,T](operator,term)
    def unapply(term:Term[T]) = term match {
      case Quantified(operator,fun) => Option(fun)
      case _ => None
    }
  }

  object Exists extends AbstractQuantified[Boolean] { def operator = ConstantFun(Math.Or)}
  object Forall extends AbstractQuantified[Boolean] { def operator = ConstantFun(Math.And)}
  object VecSum extends AbstractQuantified[Vec] { def operator = ConstantFun(Math.VecAdd)}





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
 * Set of all vectors.
 */
case object Vecs extends AllObjects[Vec] {
  def iterator = Util.tooLargeToIterate
  override def size = Util.tooLargeToCount
  override def head = Vec.zero
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

class AllOfType[T] extends AllObjects[T] {
  def iterator = Util.tooLargeToIterate
}

case object All extends AllObjects[Any] {
  def iterator = Util.tooLargeToIterate
}

case object AllRefs extends AllObjects[AnyRef] {
  def iterator = Util.tooLargeToIterate
}



