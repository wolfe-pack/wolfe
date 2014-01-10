package scalapplcodefest.newExamples

import scalapplcodefest.Wolfe

/**
 * @author Sebastian Riedel
 */
class CoinTossing extends (() => Wolfe.Vector) {

  import Wolfe._

  type Coin = Symbol

  def apply() = {

    //elements of the domain
    val coins = Set('H, 'T)

    //training data
    val data = Seq('H, 'T, 'T, 'T)

    //MLE estimate
    argmin(vectors) {w => sum(data) {t => logZ(coins)(c => ft(c) dot w) - (ft(t) dot w)}}
  }
}
