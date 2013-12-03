package scalapplcodefest

/**
 * A state maps variables to values.
 *
 * @author Sebastian Riedel
 */
trait State {

  self =>

  /**
   * Value assigned to the variable, or `None` if no value is assigned.
   * @param variable the variable to get the value for.
   * @tparam T type of value of variable.
   * @return `Some(value)` if `variable` is mapped to `value`, `None` otherwise.
   */
  def get[T](variable: Variable[T]): Option[T]

  /**
   * Syntactic sugar for when we know that a term is defined.
   */
  def apply[T](variable: Variable[T]) = get(variable).get

  /**
   * All variables for which the state defines a value.
   * @return the domain of this state.
   */
  def domain: Set[Variable[Any]]

  /**
   * Overlays this state over the given state. This may not be a good idea to use when adding several states.
   * @param state the state to "underlay".
   * @return A state that returns the value assigned to the variable, if such value exists,
   *         or the value assigned to the variable in the passed state.
   */
  def +(state: State) = new State {
    def get[V](variable: Variable[V]) = self.get(variable).orElse(state.get(variable))
    def domain = self.domain ++ state.domain
  }

  /**
   * By default states have open world semantics: variables without an assignment return None as result
   * of calling get. This method returns a closed world version of a state: for unassigned variables
   * get returns Some([default state for that variable].
   * @return a closed world version of the given state.
   */
  def closed(vars: Set[Variable[Any]] = AllVariables) = new State {
    def get[V](variable: Variable[V]) = self.get(variable).orElse(if (domain(variable)) Some(variable.default) else None)
    override def domain = SetUtil.SetUnion(List(self.domain, vars))
  }

  /**
   * If a variable v has no assigned variable, this state returns its assigned Target value as
   * denoted by the Target(v) variable binding, if it exists.
   * @return a target state.
   */
  def target = new State {
    def get[V](variable: Variable[V]) = self.get(variable).orElse(self.get(Target(variable)))
    override def domain = self.domain.map({case Target(v) => v; case v => v})
  }

  /**
   * Hides variables (matching the predicate) and turns them into target variables. That is,
   * accessing the state for a variable v that is hidden yields None, but accessing
   * Target(v) returns the original result
   * @param shouldBeTarget predicate to test whether variable should be a target.
   * @return a state where the given variables have become target variables.
   */
  def asTargets(shouldBeTarget: Set[Variable[Any]]): State = new State {
    def get[V](variable: Variable[V]) = variable match {
      case Target(v) if shouldBeTarget(v) => self.get(v)
      case v => if (shouldBeTarget(v)) None else self.get(v)
    }
    override def domain = self.domain.map(v => if (shouldBeTarget(v)) Target(v) else v)
  }

  /**
   * Convenience method for when the targets are all ground atoms.
   */
  def asTargets(shouldBeTarget: Predicate[_, _]*): State = asTargets(SetUtil.SetUnion(shouldBeTarget.map(AllGroundAtoms(_)).toList))

  /**
   * Returns the belief for a variable `x` as denoted by state's assignment to `Belief(x)`
   * @param variable the variable to get the belief for.
   * @tparam T type of variable.
   * @return the value assigned to `Belief(variable)` if defined, otherwise an exception is thrown.
   */
  def belief[T](variable: Variable[T]):Fun[T,Double] = apply(Belief(variable))

  /**
   * Equals based on having the same variables with same values.
   * @param that the other state to compare to.
   * @return true iff both states have same variables which are mapped to the same values.
   */
  override def equals(that: scala.Any) = that match {
    case s: State if s eq this => true
    case s: State => s.domain.size == domain.size && domain.forall(v => get(v) == s.get(v))
    case _ => false
  }

  override def toString = {
    domain.toSeq.sorted(VariableOrdering).map(v => "%s->%s".format(v, get(v).get)).mkString(",")
  }

  def toPrettyString = {
    domain.toSeq.sorted(VariableOrdering).map(v => "%30s -> %s".format(v, get(v).get)).mkString("\n")
  }


}

/**
 * State companion object.
 */
object State {
  def apply(map: Map[Variable[Any], Any]): State = new MapBasedState(map)

  val empty = new State {
    def get[T](variable: Variable[T]) = None
    def domain = Set.empty
    override def +(state: State) = state
  }


  def allStates(vars: List[Variable[Any]], toConjoinWith: Seq[State] = Seq(State.empty).view): Seq[State] = {
    vars match {
      case Nil => toConjoinWith
      case head :: tail =>
        val newStates = for (oldState <- toConjoinWith; value <- head.domain.eval().get.view) yield
          oldState + SingletonState(head, value)
        allStates(tail, newStates)
    }
  }
}

/**
 * State based on an immutable map.
 * @param map the map the state is based on.
 */
class MapBasedState(val map: Map[Variable[Any], Any]) extends State {
  def get[T](variable: Variable[T]) = map.get(variable).asInstanceOf[Option[T]]
  def domain = map.keySet
}

/**
 * A state with exactly one variable->value mapping.
 * @param variable the variable to be set
 * @param state the value of the variable
 * @tparam Value the type of value the variable takes on.
 */
case class SingletonState[Value](variable: Variable[Value], state: Value) extends State {
  def get[V](variable: Variable[V]) =
    if (variable == this.variable) Some(state.asInstanceOf[V]) else None

  override def toString = variable + " = " + state
  override def domain = Set(variable)
}

