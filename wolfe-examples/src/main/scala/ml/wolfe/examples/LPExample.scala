package ml.wolfe.examples

/**
 * @author Sebastian Riedel
 */
object LPExample {

  case class Solution(x:Double, y:Double, z:Double)

  def constraints(s:Solution) = {
    import s._
    x + y <= 1.0 &&
    x + z <= 1.0
  }
  def objective(s:Solution) = {
    import s._
    1.2 * x + 0.5 * y - 1.3 * z
  }



}
