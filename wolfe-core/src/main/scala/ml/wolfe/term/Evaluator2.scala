package ml.wolfe.term

/**
 * @author riedel
 */
trait Evaluator2 {
  val input:Settings
  val output:Setting
  def eval()(implicit execution:Execution)
}

case class Execution(num:Int)

trait Differentiator2 {
  val input:Settings
  val output:Setting
  val gradientAccumulator:Settings
  val error:Setting

  def forward()(implicit execution:Execution)
  def backward()(implicit execution:Execution)

  def differentiate()(implicit execution:Execution = Execution(0)): Unit = {
    forward()
    backward()
  }

}

