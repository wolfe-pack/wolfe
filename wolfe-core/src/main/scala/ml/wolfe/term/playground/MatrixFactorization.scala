package ml.wolfe.term.playground

import cc.factorie.la.{DenseTensor2, DenseTensor1}
import ml.wolfe.FactorieVector
import ml.wolfe.term.TermImplicits._
import ml.wolfe.term._
import ml.wolfe.util.Math._

import scala.util.Random

/**
 * @author rockt
 */
object MatrixFactorization extends App {
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

  val k = 5
  val numRows = (pos ++ neg).map(_._1).max + 1
  val numCols = (pos ++ neg).map(_._2).max + 1

  val Theta = seqs(vectors(k), numRows) x seqs(vectors(k), numCols)

  val rowReg: DoubleTerm = -0.01
  val colReg: DoubleTerm = -0.01

  def cell(scale: Double)(a: VectorTerm, v: VectorTerm) =
    sum(log(sigm((a dot v) * scale)), rowReg * (a dot a), colReg * (v dot v))

  def loss(positive: Seq[(Int, Int)], negative: Seq[(Int, Int)])(t: Theta.Term) =
    sum(positive) { case (i, j) => cell(1.0)(t._1(i), t._2(j))} + sum(negative) { case (i, j) => cell(-1.0)(t._1(i), t._2(j))}

  val random = new Random(1984l)
  def nextDouble = random.nextGaussian() * 0.1
  val init = Array(Theta.createZeroSetting())
  init.head.vect.foreach(x => (0 until x.length).foreach(i => x.update(i, nextDouble)))

  val argmaxTerm = argmax(Theta) { x => loss(pos, neg)(x).argmaxBy(Argmaxer.ascent(1000, 0.1)(init)) }
  val (rows, cols) = argmaxTerm.eval()

  rows.foreach(row => {
    cols.foreach(col => {
      print("%5.2f\t".format(sigmoid(row dot col)))
    })
    println()
  })
}

object MatrixFactorizationCheck extends App {
  val k = 10

  val a = vectors(k).variable("a")
  val v = vectors(k).variable("v")

  val rowReg: DoubleTerm = -0.01
  val colReg: DoubleTerm = -0.01

  val scale: DoubleTerm = -1.0

  GradientChecking(sum(log(sigm((a dot v) * scale)), rowReg * (a dot a), colReg * (v dot v)), debug = true)()
}

object Scratch extends App {
  /*
  val v1: VectorTerm = vector(1, 2, 3)
  val v2: VectorTerm = vector(4, 5)
  val M1: MatrixTerm = matrix(Seq(1, 0, 0, 0.5, 0), Seq(0.5, 0, 0, 0, 1))

  println((M1 *(v1 cons v2)).eval())

  val v3 = vectors(3).variable("v3 ")
  val v4 = vectors(2).variable("v4 ")
  */

  val M2 = matrices(2,5).variable("M2 ")
  val v5 = vectors(5).variable("v5 ")

  GradientChecking(M2 * v5, debug = true)()
  //GradientChecking(M2 * (v3 cons v4), debug = true)()

  /*
  import ml.wolfe.util.PimpMyFactorie._
  val v6 = new DenseTensor1(Array(1.0, 2.0))
  val M3 = new DenseTensor2(Seq(Seq(1.0, 2.0, 3.0, 4.0, 5.0), Seq(6.0, 7.0, 8.0, 9.0, 10.0)))
  println((M3.t * v6).toPrettyString)
  */
}