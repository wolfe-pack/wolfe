package scalapplcodefest

/**
 * Variables are terms that get replaced by values assignment to them in states.
 * @author Sebastian Riedel
 */
trait Variable[+T] extends Term[T] {

  /**
   * The domain of the variable. This domain is dynamic, in the sense that it is a term that
   * can evaluate to different sets in different states. This is useful because models often involve domains
   * that depend on the size or shape of the input data. For example, the number of tokens when doing PoS tagging
   * depends on the sentence length.
   * @return the term that describes the domain of a variable.
   */
  def domain[C >: T]: Term[Set[C]]

  /**
   * The denotation of a variable.
   * @param state the state object that binds variables to values.
   * @return `Right(value)` if `variable->value` in `state`, [[scalapplcodefest.Variable#domain]] is defined in
   *         `state` as `dom` and `dom(value)` holds. Else `Left(this)`.
   */
  def eval(state: State) = {
    for (dom <- domain.eval(state).right; value <- state.get(this).filter(dom).toRight(this).right) yield value
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
case class Var[T](name: Symbol, dom: Term[Set[T]]) extends Variable[T] {
  def domain[C >: T] = dom.asInstanceOf[Term[Set[C]]]
  override def toString = name.name
  override def equals(v: scala.Any) = v match {
    case Var(n,_) => name == n
    case _ => false
  }
  override def hashCode() = name.hashCode()
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