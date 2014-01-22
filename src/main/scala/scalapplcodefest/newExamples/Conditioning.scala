package scalapplcodefest.newExamples

import scalapplcodefest.Wolfe
import scalapplcodefest.sbt.Compile

/**
 * @author Sebastian Riedel
 */
@Compile
class Conditioning extends (() => Boolean) {

  import Wolfe._

  type Coin = Symbol

  def apply() = {

    case class Data(x: Double, y: Boolean)

    val allData = all(Data)(c(doubles, bools))

    val best = argmax(allData)(_.x == 1.0)(_ => 1.0)

    best.y
  }
}
