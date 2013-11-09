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
  def domain: Term[Iterable[T]]

  /**
   * The denotation of a variable.
   * @param state the state object that binds variables to values.
   * @return `Some(value)` if `variable->value` in `state`, [[scalapplcodefest.Variable#domain]] is defined in
   *        `state` as `dom` and `dom(value)` holds. Else `None`.
   */
  def eval(state: State) = {
    for (dom <- domain.eval(state); value <- state.get(this) if dom.toSet(value)) yield value
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
