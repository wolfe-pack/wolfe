package ml.wolfe.legacy

import scala.collection.mutable
import ml.wolfe.legacy.term._

/**
 * @author Sebastian Riedel
 */
object DomainCollector {

  /**
   * Collects values for set-based variables (e.g. dynamic domains).
   * @param states the states to collect the values from.
   * @return a state that maps set-based variables to sets of values based on the assignments in the given states.
   */
  def collect(states: Seq[State]) = {
    val result = new mutable.HashMap[Variable[Any], Set[Any]]
    def add[T](term: Term[Set[T]], value: T) {
      (term, value) match {
        case (v: Variable[_], _) => result(v) = result.getOrElse(v, Set.empty) + value
        case (CartesianProductTerm2(d1, d2), (v1, v2)) => {
          add(d1, v1); add(d2, v2)
        }
        case _ =>
      }
    }
    for (state <- states) {
      for (variable <- state.domain) {
        variable match {
          case Var(_,dom:Variable[_]) => add(dom,state(variable))
          case GroundAtom(Predicate(_, dom, range), arg) => {add(dom,arg); add(range,state(variable))}
          case _ =>
        }
      }
    }
    State(result.toMap)
  }


}
