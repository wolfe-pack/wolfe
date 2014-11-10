package ml.wolfe.fg20

/**
 * A state maps Vars to values.
 *
 * @author Sebastian Riedel
 */
trait State {

  self =>

  /**
   * Value assigned to the Var, or `None` if no value is assigned.
   * @param Var the Var to get the value for.
   * @tparam T type of value of Var.
   * @return `Some(value)` if `Var` is mapped to `value`, `None` otherwise.
   */
  def get[T](Var: Var[T]): Option[T]

  /**
   * Syntactic sugar for when we know that a term is defined.
   */
  def apply[T](Var: Var[T]) = get(Var).get

  /**
   * All Vars for which the state defines a value.
   * @return the domain of this state.
   */
  def domain: Set[Var[Any]]

  /**
   * Overlays this state over the given state. This may not be a good idea to use when adding several states.
   * @param state the state to "underlay".
   * @return A state that returns the value assigned to the Var, if such value exists,
   *         or the value assigned to the Var in the passed state.
   */
  def +(state: State) = new State {
    def get[V](Var: Var[V]) = self.get(Var).orElse(state.get(Var))
    def domain = self.domain ++ state.domain
  }

  /**
   * Returns the belief for a Var `x` as denoted by state's assignment to `Belief(x)`
   * @param Var the Var to get the belief for.
   * @tparam T type of Var.
   * @return the value assigned to `Belief(Var)` if defined, otherwise an exception is thrown.
   */
  def discBelief[T](Var: DiscVar[T]):DiscDistribution[T] = apply(DiscBelief(Var))

  /**
   * Equals based on having the same Vars with same values.
   * @param that the other state to compare to.
   * @return true iff both states have same Vars which are mapped to the same values.
   */
  override def equals(that: scala.Any) = that match {
    case s: State if s eq this => true
    case s: State => s.domain.size == domain.size && domain.forall(v => get(v) == s.get(v))
    case _ => false
  }

  override def toString = {
    domain.view.map(v => "%s->%s".format(v, get(v).get)).mkString(",")
  }

  def toPrettyString = {
    domain.view.map(v => "%30s -> %s".format(v, get(v).get)).mkString("\n")
  }

}


trait Distribution[T] {
  def prob(value:T):Double
}

object Distribution {
  def disc[T](dom:Seq[T],belief:Array[Double]) =
    DiscDistribution(dom, dom.zipWithIndex.map( p => p._1 -> belief(p._2)).toMap)
}

case class DiscDistribution[T](domain:Seq[T], map:Map[T,Double]) extends Distribution[T] {
  def prob(value: T) = map(value)
}



case class DiscBelief[T](variable:DiscVar[T]) extends Var[DiscDistribution[T]]


/**
 * State companion object.
 */
object State {
  def apply(map: Map[Var[Any], Any]): State = new MapBasedState(map)

  val empty = new State {
    def get[T](Var: Var[T]) = None
    def domain = Set.empty
    override def +(state: State) = state
  }

  def single[T](Var: Var[T], value: T): State = SingletonState(Var, value)

  def allStates(vars: List[DiscVar[Any]], toConjoinWith: Seq[State] = Seq(State.empty).view): Seq[State] = {
    vars match {
      case Nil => toConjoinWith
      case head :: tail =>
        val newStates = for (oldState <- toConjoinWith; value <- head.dom.view) yield
          oldState + SingletonState(head, value)
        allStates(tail, newStates)
    }
  }
}

/**
 * State based on an immutable map.
 * @param map the map the state is based on.
 */
class MapBasedState(val map: Map[Var[Any], Any]) extends State {
  def get[T](Var: Var[T]) = map.get(Var).asInstanceOf[Option[T]]
  def domain = map.keySet
}

/**
 * A state with exactly one Var->value mapping.
 * @param Var the Var to be set
 * @param state the value of the Var
 * @tparam Value the type of value the Var takes on.
 */
case class SingletonState[Value](Var: Var[Value], state: Value) extends State {
  def get[V](Var: Var[V]) =
    if (Var == this.Var) Some(state.asInstanceOf[V]) else None

  override def toString = Var + " = " + state
  override def domain = Set(Var)
}

