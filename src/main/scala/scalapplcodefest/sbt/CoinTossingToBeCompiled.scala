package scalapplcodefest.sbt

import scalapplcodefest.Wolfe

/**
 * @author Sebastian Riedel
 */
object CoinTossingToBeCompiled extends App {

  import Wolfe._

  type Coin = Symbol

  //elements of the domain
  val coins = Set('H, 'T)

  //training data
  val data = Seq('H, 'T, 'T, 'T)

  val myReals = Set(0.0, 0.25, 0.75, 1.0).map(math.exp)

  //val w = argmin(vectors)(ll(data)) //this won't run without compilation

  val w = argmin(vectors(coins, myReals)) {w => sum(data) {gold => logZ(coins)(c => ft(c) dot w) - (ft(gold) dot w)} } //this should run w/o compilation


}
