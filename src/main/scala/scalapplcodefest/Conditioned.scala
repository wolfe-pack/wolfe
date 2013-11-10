package scalapplcodefest

/**
 * @author Sebastian Riedel
 */
case class Conditioned[T](term:Term[T], condition:State) extends Term[T] {
  def eval(state: State) = term.eval(state + condition)
  def variables = term.variables //todo: should be clever in removing
  def default = term.default
  def domain[C >: T] = term.domain[C]
}
