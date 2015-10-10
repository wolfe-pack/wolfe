package ml.wolfe.term.simplified

/**
 * A Term is an expression that evaluates to a specific value given a binding to the free variables in the expression.
 * @author riedel
 */
trait STerm[+T]

/**
 * A term that is composed of sub-terms.
 */
trait Composed extends STerm[Any] {
  /**
   * The sub-terms of this term.
   * @return a sequence of terms that determine the semantics of this term.
   */
  def parts: Seq[STerm[Any]]
}

/**
 * A constant is a term that always evaluates to the same value, independent of any variable bindings.
 * @param value the value the term evaluates to.
 * @tparam T the type of value it represents.
 */
case class Constant[+T](value: T) extends STerm[T]

/**
 * A variable evaluates to the value it is bound to in a variable binding. This is not a case class, meaning
 * that two variables that are different objects but have the same name will not be equal.
 * @param name the name of the variable.
 * @tparam T the type of values the variable can be bound to.
 */
class Var[+T](val name: String) extends STerm[T] {
  override def toString = name
}

/**
 * A Term that represents the apply/element access operation to a Seq
 * @param s the term that represents the sequence
 * @param i the term that represents the integer index.
 * @tparam E the type of elements in the sequence.
 */
case class SeqApply[+E](s: STerm[Seq[E]], i: STerm[Int]) extends STerm[E] with Composed {
  def parts = Seq(s, i)
}

/**
 * A term that represents the original sequence with an element appended to its end.
 * @param s the original sequence.
 * @param elem the element to append
 * @tparam E the type of elements in the sequence.
 */
case class SeqAppend[+E](s: STerm[Seq[E]], elem: STerm[E]) extends STerm[Seq[E]] with Composed {
  def parts = Seq(s, elem)
}

/**
 * Applies a fold-left on a term representing a sequence.
 * @param s the term representing the sequence to apply the fold-left to.
 * @param init the initial result passed into the first application of the operator.
 * @param op the operator that transforms the current result and the current element into the next result.
 * @tparam E the type of elements in the sequence.
 * @tparam S the type of the result.
 */
case class SeqFoldLeft[E, S](s: STerm[Seq[E]], init: STerm[S], op: (STerm[S], STerm[E]) => STerm[S]) extends STerm[S]

case class SeqScanLeft[E, S](s: STerm[Seq[E]], init: STerm[S], op: (STerm[S], STerm[E]) => STerm[S]) extends STerm[Seq[S]]

case class SeqFill[+E](length: STerm[Int], element: STerm[E]) extends STerm[Seq[E]]

case class SeqPointWiseMax(s1: STerm[Seq[Double]], s2: STerm[Seq[Double]]) extends STerm[Seq[Double]]

case class SeqMinus(s1: STerm[Seq[Double]], s2: STerm[Seq[Double]]) extends STerm[Seq[Double]]

case class RangeTerm(from: STerm[Int], to: STerm[Int]) extends STerm[Seq[Int]]

case class SeqSlice[+E](s: STerm[Seq[E]], from: STerm[Int], to: STerm[Int]) extends STerm[Seq[E]]

/**
 * Represents the map operation on sequences.
 * @param s the sequence to map over.
 * @param f a function that takes a term representing a sequence element, and returns a term that
 *          represents the mapped element.
 * @tparam A original element type in the sequence.
 * @tparam B new element type.
 */
case class SeqMap[A, +B](s: STerm[Seq[A]], f: STerm[A] => STerm[B]) extends STerm[Seq[B]]

/**
 * A term that represents the length of a sequence.
 * @param s the sequence of which we want to return the length.
 * @tparam E the element type of the sequence.
 */
case class SeqLength[+E](s: STerm[Seq[E]]) extends STerm[Int]

/**
 * A term that represents the contruction of a sequence based on argument terms.
 * @param args a sequence of terms that are evaluated to become the arguments of the result sequence.
 * @tparam E the type of elements in the sequence.
 */
case class SeqConstructor[+E](args: Seq[STerm[E]]) extends STerm[Seq[E]]

/**
 * A term that represents the element-th element of a product value.
 * @param product the product of which we want to return an element.
 * @param element the index of the element to return.
 * @tparam T the type of the element to return.
 */
case class GetElement[+T](product: STerm[Product], element: Int) extends STerm[T]

/**
 * A term representing the construction of a product value.
 * @param args the argument terms that make up the product elements.
 * @param constructor a constructor that takes the arguments and returns the actual product value.
 * @tparam T the type of the product.
 */
case class ConstructProduct[+T <: Product](args: Seq[STerm[Any]], constructor: Seq[Any] => T) extends STerm[T]

/**
 * A composed term with two arguments.
 * @tparam T1 the value type of the first term.
 * @tparam T2 the value type of the second term.
 */
trait BinaryOperation[T1, T2] extends Composed {
  def arg1: STerm[T1]

  def arg2: STerm[T2]

  def parts = Seq(arg1, arg2)
}

case class Plus[N](arg1: STerm[N], arg2: STerm[N])(implicit val numeric: Numeric[N]) extends STerm[N] with BinaryOperation[N, N]

case class Minus[N](arg1: STerm[N], arg2: STerm[N])(implicit val numeric: Numeric[N]) extends STerm[N] with BinaryOperation[N, N]

case class Sum[N](args: STerm[Seq[N]])(implicit val numeric: Numeric[N]) extends STerm[N]

case class Times[N](arg1: STerm[N], arg2: STerm[N])(implicit val numeric: Numeric[N]) extends STerm[N] with BinaryOperation[N, N]

trait Vect
trait Mat

case class MatVectProduct(A:STerm[Mat], x:STerm[Vect]) extends STerm[Vect]
case class Tanh(x:STerm[Vect]) extends STerm[Vect]
case class VectPlus(x1:STerm[Vect], x2:STerm[Vect]) extends STerm[Vect]

case class Sigmoid(x:STerm[Vect]) extends STerm[Vect]
case class ComponentMult(x1:STerm[Vect], x2:STerm[Vect]) extends STerm[Vect]


