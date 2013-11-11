package scalapplcodefest

/**
 * @author Sebastian Riedel
 */
case class Conditioned[T](term:Term[T], condition:State) extends Term[T] {
  def eval(state: State) = term.eval(state + condition)
  def variables = {
    def conditionVars(vars: Set[Variable[Any]]): Set[Variable[Any]] = vars match {
      case SetUtil.Union(args) => SetUtil.Union(args.map(conditionVars))
      case AllGroundAtoms(pred,state) => AllGroundAtoms(pred,state + condition)
      case _ => vars
    }
    conditionVars(term.variables)
  }
  def default = term.default
  def domain[C >: T] = term.domain[C]
}
