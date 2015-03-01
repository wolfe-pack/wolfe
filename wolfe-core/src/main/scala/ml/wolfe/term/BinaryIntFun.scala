package ml.wolfe.term

/**
 * @author riedel
 */
class BinaryIntFun[T1 <: IntTerm, T2 <:IntTerm](val a1:T1, val a2:T2, fun:(Int,Int) => Int) extends Composed[IntDom] {
  val domain = Dom.ints
  type ArgumentType = IntTerm
  val arguments = IndexedSeq(a1,a2)
  def copy(args: IndexedSeq[ArgumentType]) = new BinaryIntFun[T1,T2](args(0).asInstanceOf[T1],args(1).asInstanceOf[T2],fun)


  override def composer2(args: Settings) = new Composer2(args) {
    def eval()(implicit execution: Execution) = {
      output.disc(0) = fun(input(0).disc(0),input(1).disc(0))
    }
  }

  def composer() = ???
  def differentiator(wrt: Seq[Var[Dom]]) = ???
}