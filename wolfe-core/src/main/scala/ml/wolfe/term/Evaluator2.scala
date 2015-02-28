package ml.wolfe.term

/**
 * @author riedel
 */
trait Evaluator2 {
  val input:Settings
  val output:Setting
  def eval()
}

trait Differentiator2 {
  val input:Settings
  val output:Setting
  val gradientAccumulator:Settings
  val error:Setting

  def forward()
  def backward()

  def differentiate(): Unit = {
    forward()
    backward()
  }

}

