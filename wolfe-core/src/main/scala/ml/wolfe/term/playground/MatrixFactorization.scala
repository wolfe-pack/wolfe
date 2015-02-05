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

object NeuralTensorFactorization extends App {
  //tensor
  val pos = Seq(
    (0 -> 0) -> 0, (0 -> 0) -> 1,
    (0 -> 1) -> 0,
    (1 -> 1) -> 1
  )

  val neg = Seq(
    (0 -> 1) -> 1,
    (1 -> 1) -> 0
  )

  val k = 10
  val num_c1 = (pos ++ neg).map(_._1._1).max + 1
  val num_c2 = (pos ++ neg).map(_._1._2).max + 1
  val num_r =  (pos ++ neg).map(_._2).max + 1 

  val Theta = seqs(vectors(k), num_c1) x seqs(vectors(k), num_c2) x seqs(vectors(k), num_r) x matrices(k, 2 * k)

  val c1Reg: DoubleTerm = -0.01
  val c2Reg: DoubleTerm = -0.01
  val rReg: DoubleTerm =  -0.01
  val WCReg: DoubleTerm = -0.01

  def cell(scale: Double)(r: VectorTerm, c1: VectorTerm, c2: VectorTerm, WC: MatrixTerm) = sum(
    log(sigm((r dot (WC * (c1 cons c2))) * scale)),
    rReg * (r dot r), 
    c1Reg * (c1 dot c1), 
    c2Reg * (c2 dot c2), 
    WCReg * (WC dot WC)
  )

  def loss(positive: Seq[((Int, Int), Int)], negative: Seq[((Int, Int), Int)])(t: Theta.Term) = {
    val w: MatrixTerm =  t._2
    sum(positive) { case ((i, j), s) => cell(1.0)(t._1._2(s), t._1._1._1(i), t._1._1._2(j), w)} +
      sum(negative) { case ((i, j), s) => cell(-1.0)(t._1._2(s), t._1._1._1(i), t._1._1._2(j), w)}
  }

  val random = new Random(1984l)
  def nextDouble = random.nextGaussian() * 0.1
  val init = Array(Theta.createZeroSetting())
  init.head.vect.foreach(x => (0 until x.length).foreach(i => x.update(i, nextDouble)))
  init.head.mats.foreach(x => (0 until x.length).foreach(i => x.update(i, nextDouble)))
  
  val argmaxTerm = argmax(Theta) { x => loss(pos, neg)(x).argmaxBy(Argmaxer.ascent(1000, 0.1)(init)) }
  val (((c1s, c2s), rs), w) = argmaxTerm.eval()

  def cons(v1: FactorieVector, v2: FactorieVector) =
    new DenseTensor1((v1.asSeq ++ v2.asSeq).toArray)

  (pos ++ neg).foreach(t => {
    val ((c1Ix, c2Ix), rIx) = t
    val (c1, c2, r) = (c1s(c1Ix), c2s(c2Ix), rs(rIx))
    println(s"($c1Ix, $c2Ix) -> $rIx\t" + "%5.2f\t".format(sigmoid(r dot (w * cons(c1, c2)))))
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

object NeuralTensorFactorizationCheck extends App {
  val k = 5

  val r = vectors(k).variable("r ")
  val c1 = vectors(k).variable("c1 ")
  val c2 = vectors(k).variable("c2 ")
  val WC = matrices(k, 2*k).variable("WC ")

  val rReg: DoubleTerm =  -0.01
  val c1Reg: DoubleTerm = -0.01
  val c2Reg: DoubleTerm = -0.01
  val WCReg: DoubleTerm = -0.01

  val scale: DoubleTerm = -1.0

  GradientChecking(sum(
    log(sigm((r dot (WC * (c1 cons c2))) * scale)),
    rReg * (r dot r),
    c1Reg * (c1 dot c1),
    c2Reg * (c2 dot c2),
    WCReg * (WC dot WC)
  ), debug = true)()
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