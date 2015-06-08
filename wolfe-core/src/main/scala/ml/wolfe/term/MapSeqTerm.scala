package ml.wolfe.term

import ml.wolfe.term.TermImplicits._
/**
 * @author sameer
 * @since 6/8/15.
 */
case class MapSeqTerm[D <: Dom, To <: Dom,  R <: Term[VarSeqDom[D]], Body <: Term[To]](range: R, variable: Var[D], body: Body)
  extends Term[VarSeqDom[To]] with NAry {
  override val domain: VarSeqDom[To] = new VarSeqDom(body.domain, range.domain.maxLength, range.domain.minLength)

  override def isStatic: Boolean = range.isStatic && body.isStatic

  override def vars: Seq[AnyVar] = body.vars ++ range.vars ++ Seq(variable)

  override def arguments: IndexedSeq[ArgumentType] = IndexedSeq(range,body)

  override def copy(args: IndexedSeq[ArgumentType]): Term[Dom] =
    MapSeqTerm[D,To,R,Body](args(0).asInstanceOf[R],variable, args(1).asInstanceOf[Body])

  override type ArgumentType = AnyTerm
}
