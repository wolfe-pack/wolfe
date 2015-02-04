package ml.wolfe.term.playground

import ml.wolfe.term.TermImplicits._
import ml.wolfe.term._
import ml.wolfe.util.Math._

import scala.util.Random

/**
 * @author rockt
 */
object MatrixFactorization extends App {
  val k = 10
  val numRows = 3
  val numCols = 4
  val Params = seqs(vectors(k), numRows) x seqs(vectors(k), numCols)

  val rowReg: DoubleTerm = -0.01
  val colReg: DoubleTerm = -0.01

  def cell(scale: Double)(a: VectorTerm, v: VectorTerm) =
      sum(log(sigm((a dot v) * scale)), rowReg * (a dot a), colReg * (v dot v))
      //log(sigm((a dot v) * scale))


  def loss(positive: Seq[(Int, Int)], negative: Seq[(Int, Int)])(e: Params.Var) =
    sum(positive) { case (i, j) => cell(1.0)(e._1(i), e._2(j))} + sum(negative) { case (i, j) => cell(-1.0)(e._1(i), e._2(j))}


  val pos = Seq(
    0 -> 0, 0 -> 2, 0 -> 3,
    1 -> 1, 1 -> 2,
    2 -> 0, 2 -> 3
  )

  val neg = Seq(
    0 -> 1,
    1 -> 0, 1 -> 3,
    2 -> 1, 2 -> 2
  )

  val random = new Random(1984l)
  val nextDouble = random.nextGaussian() * 0.1
  val init = Array(Params.createZeroSetting())
  init.head.vect.foreach(v => (0 until v.dim1).foreach(i => v.update(i, nextDouble)))

  val argmaxTerm = argmax(Params) { x => loss(pos, neg)(x).argmaxBy(Argmaxer.ascent(1000, 0.01)(init)) }
  val (rows, cols) = argmaxTerm.eval()

  println()
  rows.foreach(row => {
    cols.foreach(col => {
      print("%5.2f\t".format(sigmoid(row dot col)))
    })
    println()
  })
}

object MatrixFactorizationCheck extends App {
  val a = vectors(10).variable("a")
  val v = vectors(10).variable("v")
  val rowReg: DoubleTerm = -0.01
  val colReg: DoubleTerm = -0.01
  val scale: DoubleTerm = -1.0
  GradientChecking(sum(log(sigm((a dot v) * scale)), rowReg * (a dot a), colReg * (v dot v)), debug = true)()
}
