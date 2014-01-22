package scalapplcodefest.example

import scalapplcodefest.Wolfe

/**
 * @author Sebastian Riedel
 */
object LPExample extends App {

  import Wolfe._

  case class Data(edge: Map[(Int, Int), Double])

  def tokens = Range(0, 3).toSet
  def edges = for (edge <- c(tokens, tokens) -> Set(0.0, 0.5, 1.0)) yield Data(edge)

  def constraints(data: Data) = {
    import data._
    forall (tokens) {m => sumOld(tokens) {h => 1.0 * edge(h, m)} == 1.0}
  }

  def obj(coeff: Map[(Int, Int), Double])(data:Data) = {
    import data._
    sumOld (c(tokens,tokens)) {case (h,m) => edge(h,m) * coeff(h,m)}
  }

  val coeffs = Map((1,3) -> 0.3) withDefaultValue 0.0

  val solution = argmaxOld(edges filter constraints) { obj(coeffs)}


}
