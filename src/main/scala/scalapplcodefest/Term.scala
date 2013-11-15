package scalapplcodefest

import scala.language.implicitConversions


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
  def eval(state: State = State.empty): Option[T]

  /**
   * Free variables in term. Notice that the returned sets can often have structure and be implicitly defined.
   * For example, a set of ground atom variables can be defined through specifying the predicate and the domain
   * of the arguments.
   * @return the free variables defined in the term and all subterms.
   */
  def variables: Set[Variable[Any]]

  /**
   * A domain of a term is a set of values such that for every possible state the term evaluates to a value that
   * is within this domain. The domain may be larger than necessary, that is, some values in the domain may not
   * ever be produced by the term. The domain itself is a term. This means that its value may be undefined
   * (because it can contain free variables).
   * @tparam C a superclass of `T`. This type parameter is introduced to maintain covariance of terms. Clients
   *           can use this parameter to, in a way, cast the domain to a set of more generic objects.
   * @return the domain of the term.
   */
  def domain[C >: T]: Term[Set[C]]

  /**
   * A default value assigned to this term. This value does not depend on a
   * state. Default values are useful to determine the type parameter of a term
   * w/o going through manifests.
   * @return the default value assigned to this term.
   */
  def default: T

}

/**
 * Proxy of another term. All methods are delegated to the inner term.
 * @tparam T type of term.
 */
trait ProxyTerm[T] extends Term[T] {
  def self:Term[T]
  def eval(state: State) = self.eval(state)
  def variables = self.variables
  def domain[C >: T] = self.domain
  def default = self.default
}

/**
 * Scala covariance/contravariance for Sets requires frequent casting of sets.
 * Import this object to make casting a little easier.
 */
object SetCastHelper {

  case class Proxy[T](term: Term[Set[T]]) {
    def as[C] = term.asInstanceOf[Term[Set[C]]]
  }

  implicit def toTermSetProxy[T](term: Term[Set[T]]) = Proxy(term)
}

/**
 * The simplest type of term always evaluates to the same value.
 * @param value the value of the constant.
 * @tparam T the type of the constant.
 */
case class Constant[T](value: T) extends Term[T] {
  def eval(state: State) = Some(value)
  def variables = Set.empty
  def domain[C >: T] = Constant(Set(value))
  def default = value
}



