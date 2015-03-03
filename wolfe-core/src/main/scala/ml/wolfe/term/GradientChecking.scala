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

  def wiggle[D](value: D, epsilon: Double, ix: Int*): (D, D) = (value match {
    case x: Double => (x + epsilon, x - epsilon)
    case x: FactorieVector =>
      val xPos = x.copy
      val xNeg = x.copy
      xPos.update(ix(0), xPos(ix(0)) + epsilon)
      xNeg.update(ix(0), xNeg(ix(0)) - epsilon)
      (xPos, xNeg)
    case x: FactorieMatrix =>
      val xPos = x.copy
      val xNeg = x.copy
      xPos.update(ix(0), ix(1), xPos(ix(0), ix(1)) + epsilon)
      xNeg.update(ix(0), ix(1), xNeg(ix(0), ix(1)) - epsilon)
      (xPos, xNeg)
  }).asInstanceOf[(D, D)]

  def apply[D <: Dom](term: Term[D], epsilon: Double = 0.00001, debug: Boolean = false)
                     (vals: Seq[Any] = term.vars.map(v => randomSetting(v)), error: term.domain.Value = term.domain.one) =
    term.vars.zipWithIndex.foreach(t => {
      val (variable, ix) = t
      val value = vals(ix)

      val gradient = term.gradient(vals, error, Seq(variable))(ix)

      //how many values we have to test
      val range = vals(ix) match {
        case x: Double => Seq(0 to 0)
        case x: FactorieVector => Seq(0 until x.dim1)
        case x: FactorieMatrix => (0 until x.dim2).map(i => 0 until x.dim1)
        case _ => ???
      }

      range.zipWithIndex.foreach(r => {
        val (rows, col) = r
        rows.foreach(row => {
          //making small changes to the variable
          val (xPos, xNeg) = wiggle(value, epsilon, row, col)
          val scorePos = term.eval(vals.updated(ix, xPos): _*)
          val scoreNeg = term.eval(vals.updated(ix, xNeg): _*)

          //numerically calculated gradient
          val dNum = (scorePos, scoreNeg) match {
            case (p: Double, n: Double) => (p - n) / (2 * epsilon)
            case (p: FactorieVector, n: FactorieVector) => ((p - n) / (2 * epsilon)).oneNorm
            case (p: FactorieMatrix, n: FactorieMatrix) => ((p - n) / (2 * epsilon)).oneNorm
            case _ => ???
          }

          //analytically calculated gradient
          val (dAna, variableName) = gradient match {
            case x: Double => (x, variable.name)
            case x: FactorieVector => (x(row), variable.name + row)
            case x: FactorieMatrix => (x(row, col), variable.name + row + "," + col)
            case _ => ???
          }

          //comparing analytical with numerical gradient
          if (debug) {
            val ratio = if (dAna === 0.0 +- epsilon && dNum === 0.0 +- epsilon) 1.0 else dNum / dAna
            println("%-6s\tana: %12.8f\tnum: %12.8f\tratio: %12.8f".format(variableName, dAna, dNum, ratio))
          } else assert(dAna === dNum +- epsilon, s"Calculated gradient $dAna does not match numerical gradient $dNum for variable $variableName!")
        })
      })
    })
}

object Scratch extends App {
  import ml.wolfe.term.TermImplicits._

  val a = Doubles.variable("a")

  val x = Vectors(3).variable("x")
  val y = Vectors(3).variable("y")
  val z = Vectors(5).variable("z")

  val A = Matrices(5,3).variable("A")

  val complexTerm = sigm(tanh(((sigmVec(A * x) * a) dot tanhVec(z)) * a))

  GradientChecking(complexTerm, debug = true)()
}