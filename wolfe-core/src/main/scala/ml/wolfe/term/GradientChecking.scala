package ml.wolfe.term

import cc.factorie.la.{DenseTensor2, DenseTensor1}
import ml.wolfe.{FactorieMatrix, FactorieVector}

import scala.util.Random
import org.scalautils.Tolerance._
import org.scalautils.TripleEquals._

/**
 * @author rockt
 */
object GradientChecking {
  var rand = new Random(1984l)

  def nextDouble = rand.nextDouble() * 2 - 1

  def randomSetting[D <: Dom](v: Var[D]) = v match {
    case x: DiscVar[_] => ???
    case x: DoubleVar => nextDouble
    case x: VectorVar => new DenseTensor1((0 until x.domain.zero.dim1).map(i => nextDouble).toArray)
    case x: MatrixVar =>
      val tmp = new DenseTensor2(x.domain.zero.dim1, x.domain.zero.dim2)
      (0 until tmp.dim1).foreach(row => (0 until tmp.dim2).foreach(col => tmp(row, col) = nextDouble))
      tmp
  }

  def apply[D <: Dom](term: Term[D], epsilon: Double = 0.00001, debug: Boolean = false)
                     (vals: Seq[Any] = term.vars.map(v => randomSetting(v)), error: term.domain.Value = term.domain.one) =
    term.vars.zipWithIndex.foreach(t => {
      val (v, ix) = t
      vals(ix) match {
        case x: Double =>
          val dAna = term.gradient(vals, error, Seq(v))(ix).asInstanceOf[Double]

          val xPos = x + epsilon
          val scorePos = term(vals.updated(ix, xPos): _*)

          val xNeg = x - epsilon
          val scoreNeg = term(vals.updated(ix, xNeg): _*)

          val dNum = (scorePos, scoreNeg) match {
            case (p: Double, n: Double) => (p - n) / (2 * epsilon)
            case (p: FactorieVector, n: FactorieVector) => ((p - n) / (2 * epsilon)).oneNorm
            case (p: FactorieMatrix, n: FactorieMatrix) => ((p - n) / (2 * epsilon)).oneNorm
            case _ => ???
          }

          if (debug) {
            val error = if (dAna === 0.0 +- epsilon && dNum === 0.0 +- epsilon) 1.0 else dNum / dAna
            println("%-6s\tana: %12.8f\tnum: %12.8f\terr: %12.8f".format(v.name, dAna, dNum, error))
          } else {
            assert(dAna === dNum +- epsilon, s"Calculated gradient $dAna does not match numerical gradient $dNum for double variable ${v.name}!")
          }


        case x: FactorieVector =>
          val dAna = term.gradient(vals, error, Seq(v))(ix).asInstanceOf[FactorieVector]

          (0 until x.dim1).foreach(i => {
            val xPos = x.copy
            xPos.update(i, xPos(i) + epsilon)
            val scorePos = term(vals.updated(ix, xPos): _*)

            val xNeg = x.copy
            xNeg.update(i, xNeg(i) - epsilon)
            val scoreNeg = term(vals.updated(ix, xNeg): _*)

            val dNum = (scorePos, scoreNeg) match {
              case (p: Double, n: Double) => (p - n) / (2 * epsilon)
              case (p: FactorieVector, n: FactorieVector) => ((p - n) / (2 * epsilon)).oneNorm
              case (p: FactorieMatrix, n: FactorieMatrix) => ((p - n) / (2 * epsilon)).oneNorm
              case _ => ???
            }

            if (debug) {
              val error = if (dAna(i) === 0.0 +- epsilon && dNum === 0.0 +- epsilon) 1.0 else dNum / dAna(i)
              println("%s%-4d\tana: %12.8f\tnum: %12.8f\terr: %12.8f".format(v.name, i, dAna(i), dNum, error))
            } else {
              assert(dAna(i) === dNum +- epsilon, s"Calculated gradient ${dAna(i)} does not match numerical gradient $dNum for vector variable ${v.name} at index $i!")
            }
          })

        case x: FactorieMatrix =>
          val dAna = term.gradient(vals, error, Seq(v))(ix).asInstanceOf[FactorieMatrix]

          (0 until x.dim1).foreach(row => (0 until x.dim2).foreach(col => {
            val xPos = x.copy
            xPos.update(row, col, xPos(row, col) + epsilon)
            val scorePos = term(vals.updated(ix, xPos): _*)


            val xNeg = x.copy
            xNeg.update(row, col, xNeg(row, col) - epsilon)
            val scoreNeg = term(vals.updated(ix, xNeg): _*)

            val dNum = (scorePos, scoreNeg) match {
              case (p: Double, n: Double) => (p - n) / (2 * epsilon)
              case (p: FactorieVector, n: FactorieVector) => ((p - n) / (2 * epsilon)).oneNorm
              case (p: FactorieMatrix, n: FactorieMatrix) => ((p - n) / (2 * epsilon)).oneNorm
              case _ => ???
            }

            if (debug) {
              val error = if (dAna(row, col) === 0.0 +- epsilon && dNum === 0.0 +- epsilon) 1.0 else dNum / dAna(row, col)
              println("%s%d%-4d\tana: %12.8f\tnum: %12.8f\terr: %12.8f".format(v.name, row, col, dAna(row, col), dNum, error))
            } else {
              assert(dAna(row, col) === dNum +- epsilon, s"Calculated gradient ${dAna(row, col)} does not match numerical gradient $dNum for matrix variable ${v.name} at index ($row,$col)!")
            }
          }))
      }
    })

}

object Scratch extends App {
  import ml.wolfe.term.TermImplicits._

  val a = doubles.variable("a")

  val x = vectors(3).variable("x")
  val y = vectors(3).variable("y")
  val z = vectors(2).variable("z")

  val A = matrices(2,3).variable("A")

  val mulTerm = a * a
  val dotTerm = x dot y
  val vecTerm = x * a
  val matTerm = A * x
  val complexTerm = (((A * x) * a) dot z) * a

  GradientChecking(complexTerm, debug = true)()
}