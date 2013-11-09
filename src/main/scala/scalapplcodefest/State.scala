package scalapplcodefest

/**
 * A state maps variables to values.
 *
 * @author Sebastian Riedel
 */
trait State {

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

}

/**
 * State companion object.
 */
object State {
  def apply(map: Map[Variable[Any], Any]): State = new MapBasedState(map)
}

/**
 * State based on an immutable map.
 * @param map the map the state is based on.
 */
class MapBasedState(val map:Map[Variable[Any],Any]) extends State {
  def get[T](variable: Variable[T]) = map.get(variable).asInstanceOf[Option[T]]
  def domain = map.keySet
}