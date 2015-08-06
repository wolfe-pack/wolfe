package ml.wolfe.term

/**
 * @author riedel
 */
class LogZ(val obj: DoubleTerm, val wrt: Var[Dom]) extends DoubleTerm with Unary {
  self =>
  override def argument: ArgumentType = obj

  override def copy(arg: ArgumentType): Term[Dom] = new LogZ(arg, wrt)

  override type ArgumentType = DoubleTerm

  val domain = Dom.doubles
  val vars = obj.vars.filterNot(_ == wrt)

  override def differentiatorImpl(diffWrt: Seq[AnyVar])(in: Settings, err: Setting, gradientAcc: Settings) =
    new ProxyDifferentiator(explicit.differentiatorImpl(diffWrt)(in, err, gradientAcc))

  lazy val explicit = {
    import TermImplicits._
    val elements = wrt.domain.toIterable.toIndexedSeq
    val Ranges = Seqs(wrt.domain, elements.length)
    val range = Ranges.Const(elements)
    val sum = FirstOrderDoubleSum(range, wrt, exp(obj))
    val logZ = log(sum)
    logZ
  }


  override def evaluatorImpl(in: Settings) =
    new ProxyEvaluator(explicit.evaluatorImpl(in))


}




