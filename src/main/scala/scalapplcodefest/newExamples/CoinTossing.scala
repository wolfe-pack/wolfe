package scalapplcodefest.newExamples

import scalapplcodefest.Wolfe
import scalapplcodefest.sbt.Compile

/**
 * @author Sebastian Riedel
 */
@Compile
class CoinTossing extends (() => Wolfe.Vector) {

  import Wolfe._

  type Coin = Symbol

  def apply() = {

    //elements of the domain
    val coins = Set('H, 'T)

    //training data
    val data = Seq('H, 'T, 'T, 'T)

    def model(c:Coin, w:Vector) = oneHot(c) dot w

    //MLE estimate
    argmin(vectors)(_ => true) {w => sumOld(data) {t => logZ(coins)(_ => true)(c => model(c,w)) - model(t, w)}}


  }
}
