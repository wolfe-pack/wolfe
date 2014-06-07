package ml.wolfe.fg

/**
 * @author Sebastian Riedel
 */
trait Msg {
  def asDiscrete = this.asInstanceOf[DiscreteMsg]
}

final class DiscreteMsg(dim:Int) extends Msg {
  val n2f     = Array.ofDim[Double](dim)
  val f2n     = Array.ofDim[Double](dim)
  val f2nLast = Array.ofDim[Double](dim)
}