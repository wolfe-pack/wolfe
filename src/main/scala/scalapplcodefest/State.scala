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
  def get[T](variable:Variable[T]):Option[T]

  /**
   * All variables for which the state defines a value.
   * @return the domain of this state.
   */
  def domain:Set[Variable[Any]]

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

  override def toString = {
    domain.map(v => "%s->%s".format(v, get(v).get)).mkString(",")
  }

  def toPrettyString = {
    domain.map(v => "%30s -> %s".format(v, get(v).get)).mkString("\n")
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
  }
}

/**
 * State based on an immutable map.
 * @param map the map the state is based on.
 */
class MapBasedState(val map:Map[Variable[Any],Any]) extends State {
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
