package scalapplcodefest

/**
 * A `Term` is a typed expression of the language. The core types of terms are constants, variables and
 * lambda abstractions. Notably, every probabilistic model itself is a term (composed of other terms).
 *
 * The meaning of a term is defined
 * through [[http://en.wikipedia.org/wiki/Denotational_semantics compositional denotational semantics]]. Given a
 * [[scalapplcodefest.State]] (or "environment") which assigns free variables to objects of type `T`,
 * a term itself denotes an object of that domain. In other words, the meaning of a term is a mapping from
 * [[scalapplcodefest.State]] objects to objects of type `T`. Usually this function is recursively defined through
 * the functions assigned to sub-parts of the term. For example, the term `x + 1` for a state `x->2` is mapped
 * to the sum of the meaning of `x` (the Int `2`) and the meaning of `1` (the Int `1`).
 *
 * Denotations of terms are defined through their [[scalapplcodefest.Term# e v a l]] method.
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




