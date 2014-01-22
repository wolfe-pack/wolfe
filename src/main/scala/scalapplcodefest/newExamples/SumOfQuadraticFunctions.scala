package scalapplcodefest.newExamples

import scalapplcodefest.Wolfe

/**
 * @author Sebastian Riedel
 */
class SumOfQuadraticFunctions extends (() => Wolfe.Vector) {

  import Wolfe._

  def apply() = {
    val offsets = Seq[Vector](Map('x1 -> 2.0, 'x2 -> -2.0))
    argminOld(vectors) {w => sumOld(offsets) {o => w dot (w + o)}}
  }

}
