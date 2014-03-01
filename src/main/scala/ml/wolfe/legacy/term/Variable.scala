package ml.wolfe.legacy.term

import org.scalautils.Good
import ml.wolfe.legacy.value.{Fun, AllOfType}
import org.scalautils.Bad

/**
 * Variables are terms that get replaced by values assignment to them in states.
 * @author Sebastian Riedel
 */
trait Variable[+T] extends Term[T] {

  /**
   * The denotation of a variable.
   * @param state the state object that binds variables to values.
   * @return `Good(value)` if `variable->value` in `state`, [[ml.wolfe.legacy.term.Variable#domain]] is defined in
   *         `state` as `dom` and `dom(value)` holds. Else undefined.
   */
  def eval(state: State) = {
    domain.eval(state) match {
      case Bad(undefined) => Bad(undefined)
      case Good(dom) => state.get(this) match {
        case Some(value) => if (dom(value)) Good(value) else Bad(ValueOutsideOfDomain(this,state))
        case None => Bad(VariableUndefined(this,state))
      }
    }
  }

  /**
   * The variable's variables.
   * @return the variable itself and the variables in the domain term.
   */
  def variables = domain.variables + this

  /**
   * The default value of a variable is the `head` of the default variable of its domain.
   * @return the default value assigned to this term.
   */
  def default = domain.default.head
}

/**
 * Simple named variable. Identity based only on name.
 * @param name name of the variable.
 * @param dom domain of the variable
 * @tparam T type of values associated with variable.
 */
case class Var[T](name: Symbol, dom: Term[Set[T]]) extends Variable[T] with Composite1[Set[T],T] {
  def domain[C >: T] = dom.asInstanceOf[Term[Set[C]]]
  override def toString = name.name
  override def equals(v: scala.Any) = v match {
    case Var(n,_) => name == n
    case _ => false
  }
  override def hashCode() = name.hashCode()
  def components = dom
  def copy(t1: Term[Set[T]]) = Var(name,t1)
  override def variables = super[Variable].variables
}

/**
 * A variable that represents the target state of another variable.
 * @param variable the variable for which this variable represents the target state.
 * @tparam V the type of the variable.
 */
case class Target[+V](variable: Variable[V]) extends Variable[V] {
  def domain[C >: V] = variable.domain
}

/**
 * A variable that represents the beliefs of another variable.
 * @param variable the variable for which this variable represents the beliefs
 * @tparam T the type of variable we need beliefs over.
 */
case class Belief[T](variable:Variable[T]) extends Variable[Fun[T,Double]] {
  def domain[C >: Fun[T, Double]] = Constant(new AllOfType[C])
}

/**
 * Defines a canonical ordering on variables.
 */
object VariableOrdering extends Ordering[Variable[Any]] {

  def compare(x1: Variable[Any], x2: Variable[Any]) = (x1, x2) match {
    case (GroundAtom(p1, a1: Int), GroundAtom(p2, a2: Int)) =>
      Ordering[(String, Int)].compare((p1.name.name, a1), (p2.name.name, a2))
    case (GroundAtom(p1, a1), GroundAtom(p2, a2)) =>
      Ordering[String].compare(p1.name.toString(), p2.name.toString())
    case (GroundAtom(p1, a1), _) => 1
    case (_, GroundAtom(p2, a2)) => -1
    case (Var(v1, _), Var(v2, _)) => Ordering[String].compare(v1.name, v2.name)
    case (Var(_, _), _) => -1
    case (_, Var(_, _)) => 1
    case (Target(v1), Target(v2)) => compare(v1, v2)
    case _ => 0
  }


}


