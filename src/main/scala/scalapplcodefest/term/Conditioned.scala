package scalapplcodefest.term

import scalapplcodefest._

/**
 * The Conditioned term add a "condition" to each state its evaluates against. This can serve as
 * mechanism to inject observations and weight vectors into more generic terms.
 * @author Sebastian Riedel
 */
case class Conditioned[T](term:Term[T], condition:State) extends Term[T] {
  def eval(state: State) = term.eval(state + condition)
  def variables = {
    def conditionVars(vars: Set[Variable[Any]]): Set[Variable[Any]] = vars match {
      case SetUtil.SetUnion(args) => SetUtil.SetUnion(args.map(conditionVars))
      case SetUtil.SetMinus(set,without) => SetUtil.SetMinus(conditionVars(set),conditionVars(without))
      case AllGroundAtoms(pred,state) => AllGroundAtoms(pred,state + condition)
      case p@PartialGroundAtoms(_,_,_) => p.copy(condition = p.condition + condition)
      case _ => vars
    }
    SetUtil.SetMinus(conditionVars(term.variables),condition.domain)
  }
  def default = term.default
  def domain[C >: T] = term.domain[C]
  val debug = variables
  val debug2 = debug.toSeq
  override def toString = "C:" + term
}


