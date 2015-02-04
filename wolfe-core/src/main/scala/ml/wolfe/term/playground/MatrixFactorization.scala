package ml.wolfe.term.playground

import ml.wolfe.term.TermImplicits._
import ml.wolfe.term._

import scala.util.Random

/**
 * @author rockt
 */
object MatrixFactorization extends App {
  val k = 2
  val numRows = 3
  val numCols = 3
  val Params = seqs(vectors(k), numRows) x seqs(vectors(k), numCols)

  val rowReg: DoubleTerm = 0.01
  val colReg: DoubleTerm = 0.01

  def cell(scale: Double)(a: VectorTerm, v: VectorTerm) =
    sum(log(sigm((a dot v) * scale)), rowReg * (a dot a), colReg * (v dot v))

  def loss(positive: Seq[(Int, Int)], negative: Seq[(Int, Int)])(e: Params.Var) =
    sum(positive) { case (i, j) => cell(1.0)(e._1(i), e._2(j))} +
      sum(negative) { case (i, j) => cell(-1.0)(e._1(i), e._2(j))}


  val pos = Seq(
    0 -> 0, 0 -> 2,
    1 -> 1, 1 -> 2,
    2 -> 0
  )

  val neg = Seq(
    0 -> 1,
    1 -> 0,
    2 -> 1, 2 -> 2
  )

  val random = new Random(1984l)
  val nextDouble = random.nextGaussian() * 0.1

  val setting = Params.createZeroSetting()
  setting.vect.foreach(v => (0 until v.dim1).foreach(i => v.update(i, nextDouble)))
  val init = Array(setting)

  val result = argmax(Params) { x => loss(pos, neg)(x).argmaxBy(Argmaxer.ascent(100, 0.1)(init))}
  result.eval()
}
