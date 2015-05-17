package ml.wolfe.term

/**
 * @author riedel
 */
trait Evaluator {
  val input:Settings
  val output:Setting
  def eval()(implicit execution:Execution)

  def tree:Seq[Evaluator] = {
    Seq(this)
  }
}

case class Execution(num:Int, typ:Execution.ExeType = Execution.Eval)

object Execution extends Enumeration {
  type ExeType = Value
  val Eval, Diff, EmptyDiff = Value



}


trait Differentiator {
  val input:Settings
  val output:Setting
  val gradientAccumulator:Settings
  val error:Setting

  def forward()(implicit execution:Execution)
  def backward()(implicit execution:Execution)

  def differentiate()(implicit execution:Execution = Execution(0,Execution.Diff)): Unit = {
    forward()
    backward()
  }

  def withRespectTo:Seq[AnyVar]


}

