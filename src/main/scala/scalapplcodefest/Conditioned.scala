package scalapplcodefest

/**
 * @author Sebastian Riedel
 */
case class Conditioned[T](term:Term[T], condition:State) extends Term[T] {
  def eval(state: State) = term.eval(state + condition)
  def variables = {
    def conditionVars(vars: Set[Variable[Any]]): Set[Variable[Any]] = vars match {
      case SetUtil.SetUnion(args) => SetUtil.SetUnion(args.map(conditionVars))
      case SetUtil.SetMinus(set,without) => SetUtil.SetMinus(conditionVars(set),conditionVars(without))
      case AllGroundAtoms(pred,state) => AllGroundAtoms(pred,state + condition)
      case _ => vars
    }
    SetUtil.SetMinus(conditionVars(term.variables),condition.domain)
  }
  def default = term.default
  def domain[C >: T] = term.domain[C]
  override def toString = "Cond:" + term
}


