package ml.wolfe.term

import ml.wolfe.Tensor

/**
 * A Term is an expression that evaluates to a specific value given a binding to the free variables in the expression.
 * @author riedel
 */
trait Term[+T]

/**
 * A term that is composed of sub-terms.
 */
trait Composed extends Term[Any] {
  /**
   * The sub-terms of this term.
   * @return a sequence of terms that determine the semantics of this term.
   */
  def parts: Seq[Term[Any]]

  def clone(args: Seq[Term[Any]]): Term[Any]
}

/**
 * Composed terms based on product values (e.g. case classes).
 */
trait ComposedProduct extends Composed {
  this: Product =>
  def parts: Seq[Term[Any]] = productIterator.map(_.asInstanceOf[Term[Any]]).toSeq

  val copyMethod = getClass.getMethods.find(_.getName == "copy").get

  def clone(args: Seq[Term[Any]]) = {
    copyMethod.invoke(this, args: _*).asInstanceOf[Term[Any]]
  }
}

/**
 * A constant is a term that always evaluates to the same value, independent of any variable bindings.
 * @param value the value the term evaluates to.
 * @tparam T the type of value it represents.
 */
case class Constant[+T](value: T) extends Term[T]

/**
 * A variable evaluates to the value it is bound to in a variable binding. This is not a case class, meaning
 * that two variables that are different objects but have the same name will not be equal.
 * @param name the name of the variable.
 * @tparam T the type of values the variable can be bound to.
 */
class Var[+T](val name: String) extends Term[T] {
  override def toString = name
}

/**
 * A Term that represents the apply/element access operation to a Seq
 * @param s the term that represents the sequence
 * @param i the term that represents the integer index.
 * @tparam E the type of elements in the sequence.
 */
case class SeqApply[+E](s: Term[Seq[E]], i: Term[Int]) extends Term[E] with ComposedProduct

/**
 * A term that represents the original sequence with an element appended to its end.
 * @param s the original sequence.
 * @param elem the element to append
 * @tparam E the type of elements in the sequence.
 */
case class SeqAppend[+E](s: Term[Seq[E]], elem: Term[E]) extends Term[Seq[E]] with ComposedProduct

/**
 * A symbolic representation of a function. 
 * @param argument the function argument.
 * @param body a body that may contain the function argument.
 * @tparam A the argument type.
 * @tparam B the return type.
 */
case class LambdaAbstraction1[A, +B](argument: Var[A], body: Term[B]) extends Term[A => B]

/**
 * Apply a lambda abstraction to an argument.
 * @param fun the function to apply.
 * @param arg the argument to apply the function to.
 * @tparam A type of arguments.
 * @tparam B type of result.
 */
case class Apply1[A, +B](fun: Term[A => B], arg: Term[A]) extends Term[B]

/**
 * Applies a fold-left on a term representing a sequence.
 * @param s the term representing the sequence to apply the fold-left to.
 * @param init the initial result passed into the first application of the operator.
 * @param op the operator that transforms the current result and the current element into the next result.
 * @tparam E the type of elements in the sequence.
 * @tparam S the type of the result.
 */
case class Foldl[E, S](s: Term[Seq[E]], init: Term[S], op: (Term[S], Term[E]) => Term[S]) extends Term[S]

case class Unfoldr[E, S](init: Term[S], op: (Term[S], Term[S]) => Term[E]) extends Term[Seq[E]]

case class MapAccumR[E, S](s: Term[Seq[E]], init: Term[S], op: (Term[S], Term[E]) => Term[S]) extends Term[Seq[S]]

case class SeqFill[+E](length: Term[Int], element: Term[E]) extends Term[Seq[E]]

case class SeqPointWiseMax(s1: Term[Seq[Double]], s2: Term[Seq[Double]]) extends Term[Seq[Double]]

case class SeqMinus(s1: Term[Seq[Double]], s2: Term[Seq[Double]]) extends Term[Seq[Double]]

case class RangeTerm(from: Term[Int], to: Term[Int]) extends Term[Seq[Int]]

case class SeqSlice[+E](s: Term[Seq[E]], from: Term[Int], to: Term[Int]) extends Term[Seq[E]]

/**
 * Represents the map operation on sequences.
 * @param s the sequence to map over.
 * @param f a function that takes a term representing a sequence element, and returns a term that
 *          represents the mapped element.
 * @tparam A original element type in the sequence.
 * @tparam B new element type.
 */
case class SeqMap[A, +B](s: Term[Seq[A]], f: LambdaAbstraction1[A, B]) extends Term[Seq[B]]

/**
 * A term that represents the length of a sequence.
 * @param s the sequence of which we want to return the length.
 * @tparam E the element type of the sequence.
 */
case class SeqLength[+E](s: Term[Seq[E]]) extends Term[Int] with ComposedProduct

/**
 * A term that represents the contruction of a sequence based on argument terms.
 * @param args a sequence of terms that are evaluated to become the arguments of the result sequence.
 * @tparam E the type of elements in the sequence.
 */
case class SeqConstructor[+E](args: Seq[Term[E]]) extends Term[Seq[E]]

/**
 * A term that represents the element-th element of a product value.
 * @param product the product of which we want to return an element.
 * @param element the index of the element to return.
 * @tparam T the type of the element to return.
 */
case class GetElement[+T](product: Term[Product], element: Int) extends Term[T]

/**
 * A term representing the construction of a product value.
 * @param args the argument terms that make up the product elements.
 * @param constructor a constructor that takes the arguments and returns the actual product value.
 * @tparam T the type of the product.
 */
case class ConstructProduct[+T <: Product](args: Seq[Term[Any]], constructor: Seq[Any] => T) extends Term[T]

/**
 * A composed term with two arguments.
 * @tparam T1 the value type of the first term.
 * @tparam T2 the value type of the second term.
 */
trait BinaryOperation[+T1, +T2, +R] extends Term[R] with ComposedProduct {
  this: Product =>
  def arg1: Term[T1]

  def arg2: Term[T2]
}

trait UnaryOperation[+T, +R] extends Term[R] with ComposedProduct {
  this: Product =>
  def arg: Term[T]
}

/**
 * Indicate that the output domain of this term is the same as the domain of the first argument.
 */
trait DomainPreserving extends Composed

/**
 * Indicate that the domains of the arguments need to be identical.
 */
trait RequiresEqualArgumentDomains extends Composed

case class Plus[N](arg1: Term[N], arg2: Term[N])(implicit val numeric: Numeric[N])
  extends Term[N] with BinaryOperation[N, N, N]

case class Minus[N](arg1: Term[N], arg2: Term[N])(implicit val numeric: Numeric[N])
  extends Term[N] with BinaryOperation[N, N, N]

case class Sum[N](args: Term[Seq[N]])(implicit val numeric: Numeric[N])
  extends Term[N]

case class Times[N](arg1: Term[N], arg2: Term[N])(implicit val numeric: Numeric[N])
  extends Term[N] with BinaryOperation[N, N, N]

case class Tanh(arg: Term[Tensor])
  extends UnaryOperation[Tensor, Tensor] with DomainPreserving

case class Sigmoid(arg: Term[Tensor])
  extends UnaryOperation[Tensor, Tensor] with DomainPreserving

case class ComponentPlus(arg1: Term[Tensor], arg2: Term[Tensor])
  extends BinaryOperation[Tensor, Tensor, Tensor] with DomainPreserving with RequiresEqualArgumentDomains

case class ComponentMul(arg1: Term[Tensor], arg2: Term[Tensor])
  extends BinaryOperation[Tensor, Tensor, Tensor] with DomainPreserving with RequiresEqualArgumentDomains

case class TensorMul(x1: Term[Tensor], x2: Term[Tensor])
  extends Term[Tensor]

case class Concat(args: Seq[Term[Tensor]])
  extends Term[Tensor]

case class Max(args: Seq[Term[Tensor]])
  extends Term[Tensor]

case class Tensor2Double(arg: Term[Tensor])
  extends UnaryOperation[Tensor, Double] with ComposedProduct

case class Transpose(arg: Term[Tensor])
  extends UnaryOperation[Tensor, Tensor]