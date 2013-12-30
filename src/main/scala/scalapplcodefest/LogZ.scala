package scalapplcodefest

import scalapplcodefest.term._
import scalapplcodefest.value.Fun
import scalapplcodefest.term.LambdaAbstraction
import scala.Some
import org.scalautils.{Bad, Good, Or}
import TermDSL._

/**
 * @author sameer
 */
object LogZ {
  def unapply(term: Term[Double]): Option[(Variable[Vector], Term[Vector])] = term match {
    case m: LogZ[_] => Some(m.parameter, m.gradient)
    case _ => None
  }

  /**
   * Computes logZ of the term by exhaustive search.
   * @param term the term to maximize.
   */
  case class ByBruteForce[T](term: LambdaAbstraction[T, Double]) extends LogZ[T] with Composite1[Fun[T, Double], Double] with DoubleTerm {
    val ForceLinear(coefficient, parameter, _) = term.body
    private var logZ: Term[Double] = _
    private var marginals: Term[Vector] = _
    private val withStateDo = new WithStateDo(state => {
      val argument = parameter.value(state)
      val body = term.body | parameter -> argument
      val coeffs = coefficient | parameter -> argument
      val (lZ, ms) = compute(body, coeffs)
      logZ = Constant(lZ)
      marginals = Constant(ms)
    })

    def compute(body: Term[Double], coeff: Term[Vector]): (Double, Vector) = {
      import StrictMath._
      val vars = body.variables
      var Z = 0.0

      for (s <- State.allStates(vars.toList)) {
        val score = exp(body.value(s))
        Z += score
      }
      var marginals: Vector = new SparseVector(1000)
      for (s <- State.allStates(vars.toList)) {
        val score = exp(body.value(s))
        val p = score / Z
        val stats = coeff.value(s)
        for ((i, d) <- stats.activeElements) {
          marginals(i) = marginals(i) + p * d
        }
      }
      assert(Z > 0.0 && !Z.isInfinite && !Z.isNaN)
      (log(Z), marginals)
    }

    def eval(state: State) = withStateDo.get(state, logZ.eval(state))
    def gradient = VectorTerm(withStateDo.get(_, marginals.value()), Set(parameter))
    def copy(t1: Term[Fun[T, Double]]) = ByBruteForce(t1.asInstanceOf[LambdaAbstraction[T, Double]])
    def components = term
  }

}

trait LogZ[T] extends Differentiable