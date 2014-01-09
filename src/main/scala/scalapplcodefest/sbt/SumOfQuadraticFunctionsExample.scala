package scalapplcodefest.sbt

import scalapplcodefest.Wolfe

/**
 * @author Sebastian Riedel
 */
object SumOfQuadraticFunctionsExample {

  import Wolfe._

  val offsets = Seq[Vector](Map('x1 -> 2.0, 'x2 -> -2.0))
  val solved = argmin(vectors) {w => sum(offsets) {o => w dot (w + o)}}

}
