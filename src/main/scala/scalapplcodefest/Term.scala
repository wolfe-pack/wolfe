package scalapplcodefest

/**
 * A `Term` is a typed expression of the language. The core types of terms are constants, variables and
 * lambda abstractions. Notably, every probabilistic model itself is a term (composed of other terms).
 *
 * The meaning of a term is defined
 * through [[http://en.wikipedia.org/wiki/Denotational_semantics compositional denotational semantics]]
 * (sometimes also known as '''mathematical semantics'''). Given a
 * [[scalapplcodefest.State]] (or "environment") which assigns free variables to objects of type `T`,
 * a term itself denotes an object of that domain. In other words, the meaning of a term is a mapping from
 * [[scalapplcodefest.State]] objects to objects of type `T`. Usually this function is recursively defined through
 * the functions assigned to sub-parts of the term. For example, the term `x + 1` for a state `x->2` is mapped
 * to the sum of the meaning of `x` (the Int `2`) and the meaning of `1` (the Int `1`).
 *
 * Denotations of terms are defined through their `eval` method.
 * @author Sebastian Riedel
 */
trait Term[+T] {

  /**
   * Defines the meaning of a term as a mapping from states to objects.
   * @param state the state object that binds variables to values.
   * @return `Some(value)` if term's `value` is defined with respect to `state`, `None` otherwise.
   */
  def eval(state: State): Option[T]

  /**
   * Free variables in term. Notice that the returned sets can often have structure and be implicitly defined.
   * For example, a set of ground atom variables can be defined through specifying the predicate and the domain
   * of the arguments.
   * @return the free variables defined in the term and all subterms.
   */
  def variables: Set[Variable[Any]]

  /**
   * A default value assigned to this term. This value does not depend on a
   * state. Default values are useful to determine the type parameter of a term
   * w/o going through manifests.
   * @return the default value assigned to this term.
   */
  def default: T

}

/**
 * The simplest type of term always evaluates to the same value.
 * @param value the value of the constant.
 * @tparam T the type of the constant.
 */
case class Constant[T](value: T) extends Term[T] {
  def eval(state: State) = Some(value)
  def variables = Set.empty
  def default = value
}

/**
 * FunctionTerm evaluate to partial functions. The domain where the function is defined for
 * may depend on the state, and is hence a term itself.
 * @tparam A type of arguments to function.
 * @tparam B type of return values of function.
 */
trait FunctionTerm[-A,+B] extends Term[PartialFunction[A,B]] {
  def domain[C<:A]:Term[Set[C]]
  def range[D>:B]:Term[Set[D]]
}

/**
 * Application of a function to an argument
 * @param function the function to apply
 * @param arg the argument to apply the function to
 * @tparam A argument type of function
 * @tparam B return type of function
 */
case class FunApp[A,B](function:FunctionTerm[A,B],arg:Term[A]) extends Term[B] {
  def eval(state: State) =
    for (f <- function.eval(state);
         a <- arg.eval(state);
         v <- f.lift(a)) yield v
  def variables = function match {
    case Predicate(_,dom,ran) => ???
    case _ => SetUtil.Union(Set(function.variables,arg.variables))
  }
  def default = function.default(function.domain.default.head)
}

/**
 * A term that is evaluated to a range of integers.
 * @param from starting integer (included)
 * @param to end integer (excluded)
 */
case class RangeSet(from:Term[Int],to:Term[Int]) extends Term[Set[Int]] {
  def eval(state: State) =
    for (f <- from.eval(state);
         t <- to.eval(state)) yield Range(f,t).toSet
  def variables = SetUtil.Union(Set(from.variables,to.variables))
  def default = Range(from.default,to.default).toSet
}


