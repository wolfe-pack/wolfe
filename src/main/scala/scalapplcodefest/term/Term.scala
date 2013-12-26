package scalapplcodefest.term

import scala.language.implicitConversions
import org.scalautils.{Bad, Good, Or}
import scalapplcodefest.value.{AllOfType, OOV}
import scalapplcodefest.TermDSL
import scalapplcodefest.SetUtil.SetUnion


/**
 * A `Term` is a typed expression of the language. The core types of terms are constants, variables and
 * lambda abstractions. Notably, every probabilistic model itself is a term (composed of other terms).
 *
 * The meaning of a term is defined
 * through [[http://en.wikipedia.org/wiki/Denotational_semantics compositional denotational semantics]]
 * (sometimes also known as '''mathematical semantics'''). Given a
 * [[scalapplcodefest.term.State]] (or "environment") which assigns free variables to objects in a domain,
 * a term itself denotes an object with Type `T` of that domain. In other words, the meaning of a term is a mapping from
 * [[scalapplcodefest.term.State]] objects to objects of type `T`. Usually this function is recursively defined through
 * the functions assigned to sub-parts of the term. For example, the term `x + 1` for a state `x->2` is mapped
 * to the sum (meaning of `+`) of the meaning of `x` (the Int `2`) and the meaning of `1` (the Int `1`).
 *
 * Denotations of terms are defined through their `eval` method.
 * @author Sebastian Riedel
 */
trait Term[+T] {

  /**
   * Defines the meaning of a term as a mapping from states to objects.
   * @param state the state object that binds variables to values.
   * @return `Good(value)` if term's `value` is defined with respect to `state`, `Bad(undefined)` otherwise.
   *         Undefined can either be variables undefined in the state, or function applications where an
   *         argument outside of the domain was used.
   */
  def eval(state: State = State.empty): T Or Undefined

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
   * (for example because it can contain free variables) without a state.
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

  /**
   * Convenience method to call when clients are sure that the term is defined.
   * @param state the state to evaluate the term against.
   * @return the value of the term in the state, if defined, otherwise an exception is thrown.
   */
  def value(state: State = State.empty): T = eval(state).get

}

/**
 * A message created when evaluating terms for which either variables or function applications are undefined.
 */
sealed trait Undefined

/**
 * During term evaluation a variable was found that had no assigned value in the state.
 * @param variable the variable without assigned value in the state.
 * @param state the state in which the variable assignment was searched for but not found.
 * @tparam T type of the variable.
 */
case class VariableUndefined[T](variable: Variable[T], state: State) extends Undefined

/**
 * During term evaluation a function was evaluated at a argument outside of the function's domain.
 * @param funApp the function application that was undefined.
 * @param state the state for which the function definition was undefined.
 * @tparam A type of arguments to function.
 * @tparam B return type of function.
 */
case class FunctionNotDefinedAt[A, B](funApp: FunApp[A, B], state: State) extends Undefined


/**
 * During term evaluation a variable was found that has a value outside of the variable's domain.
 * @param variable the variable for which the value is outside of the domain.
 * @param state the state in which the variable assignment is invalid.
 * @tparam T type of the variable.
 */
case class ValueOutsideOfDomain[T](variable: Variable[T], state: State) extends Undefined

/**
 * Companion object to create generic terms.
 */
object Term {
  def apply[T](semantics: State => T, vars: Set[Variable[Any]], defaultValue: T) = new Term[T] {
    def eval(state: State) = {
      vars.find(!state.domain(_)) match {
        case Some(v) => Bad(VariableUndefined(v, state))
        case None => Good(semantics(state))
      }
    }
    def variables = vars
    def domain[C >: T] = TermDSL.all[C]
    def default = defaultValue
  }
}

/**
 * Proxy of another term. All methods are delegated to the inner term.
 * @tparam T type of term.
 */
trait ProxyTerm[T] extends Term[T] {
  def self: Term[T]
  def eval(state: State) = self.eval(state)
  def domain[C >: T] = self.domain
  def default = self.default
  def variables = self.variables
}

/**
 * A term that will not be converted in term conversions. Note that its inner term will be converted
 * but the term itself remains atomic.
 * @param self the term this term is representing.
 * @tparam T type of term.
 */
case class Bracketed[T](self: Term[T]) extends ProxyTerm[T] with Composite1[T,T] {
  def components = self
  def copy(t1: Term[T]) = Bracketed(t1)
  override def variables = super[ProxyTerm].variables
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
  def eval(state: State) = Good(value)
  def variables = Set.empty
  def domain[C >: T] = Constant(Set(value))
  def default = value
  override def toString = value.toString
}


trait Composite[T] extends Term[T] {
  def componentSeq:Seq[Term[Any]]
  def copySeq(args:Seq[Term[Any]]):Term[T]
  def variables:Set[Variable[Any]] = SetUnion(componentSeq.toList.map(_.variables))
}
/**
 * Term that is composed of other terms.
 * @tparam T1 the type of the first argument term.
 * @tparam C the type of this term.
 */
trait Composite1[T1, C] extends Composite[C] {
  def components: Term[T1]
  def copy(t1: Term[T1]): Term[C]
  def asAny = asInstanceOf[Composite1[Any, Any]]
  def componentSeq = Seq(components)
  def copySeq(args: Seq[Term[Any]]) = copy(args(0).asInstanceOf[Term[T1]])
}

trait Composite2[T1, T2, C] extends Composite[C] {
  def components: (Term[T1], Term[T2])
  def copy(t1: Term[T1], t2: Term[T2]): Term[C]
  def asAny = asInstanceOf[Composite2[Any, Any, Any]]
  def componentSeq = Seq(components._1,components._2)
  def copySeq(args: Seq[Term[Any]]) = copy(
    args(0).asInstanceOf[Term[T1]],
    args(1).asInstanceOf[Term[T2]])

}

trait Composite3[T1, T2, T3, C] extends Composite[C] {
  def components: (Term[T1], Term[T2], Term[T3])
  def copy(t1: Term[T1], t2: Term[T2], t3: Term[T3]): Term[C]
  def asAny = asInstanceOf[Composite3[Any, Any, Any, Any]]
  def componentSeq = Seq(components._1,components._2,components._3)
  def copySeq(args: Seq[Term[Any]]) = copy(
    args(0).asInstanceOf[Term[T1]],
    args(1).asInstanceOf[Term[T2]],
    args(2).asInstanceOf[Term[T3]])

}

/**
 * Evaluates to the inner term (wrapped in `Good`) if the value is the denotation of the given domain, otherwise
 * an out-of-vocabulary element is returned.
 * @param term the term that denotes the original value before conversion to in-domain `Good` or out-of-domain.
 * @param vocab the vocabulary to check whether the value is in.
 * @tparam T type of the inner term.
 */
case class OOVTerm[T](term: Term[T], vocab: Term[Set[T]]) extends Composite2[T, Set[T], T Or OOV[T]] {
  def eval(state: State) = for (v <- term.eval(state); d <- vocab.eval(state)) yield if (d(v)) Good(v) else Bad(new OOV(v))
  def domain[A >: T Or OOV[T]] = TermDSL.all[A]
  def default = Good(term.default)
  def components = (term, vocab)
  def copy(t1: Term[T], t2: Term[Set[T]]) = OOVTerm(t1, t2)
}

