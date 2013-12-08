package scalapplcodefest

import org.scalautils.Good
import TermImplicits._
import Math._

object MultiVariate {
  def unapply(term: Term[Double]): Option[Variable[Vector]] = term match {
    case m: MultiVariate => Some(m.parameter)
    case Linear(_, p, _) => Some(p)
    case DoubleAdd.Applied2(MultiVariate(p1),MultiVariate(p2)) if p1 == p2 => Some(p1)
    case DoubleMinus.Applied2(MultiVariate(p1),MultiVariate(p2)) if p1 == p2 => Some(p1)
    case _ => None
  }
}

trait MultiVariate extends DoubleTerm {
  def parameter: Variable[Vector]
  def variables = Set(parameter)
}

object Differentiable {
  def unapply(term: Term[Double]): Option[(Variable[Vector],Term[Vector])] = term match {
    case d: Differentiable => Some(d.parameter, d.gradient)
    case MultiVariate(p) => Differentiator.differentiate(term).map(g => (p,g))
    case _ => None
  }
}

trait Differentiable extends MultiVariate {
  def gradient: Term[Vector]
}

object Max {
  def unapply(term: Term[Double]): Option[(Variable[Vector], Term[Vector], Term[State])] = term match {
    case m: Max => Some(m.parameter, m.gradient, m.argmax)
    case _ => None
  }

  /**
   * Maximizes the term by exhaustive search.
   * @param term the term to maximize.
   */
  case class ByBruteForce(term: Term[Double]) extends Max {
    val ForceLinear(coefficient, parameter, _) = term
    private var arg: State = _
    private var conditionedValue: Term[Double] = _
    private var conditionedCoefficient: Term[Vector] = _
    private val withStateDo = new WithStateDo(state => {
      val argument = state(parameter)
      conditionedValue = term | parameter -> argument
      conditionedCoefficient = coefficient | parameter -> argument
      arg = State.allStates(conditionedValue.variables.toList).view.maxBy(conditionedValue.eval(_).get)
    })
    def eval(state: State) = withStateDo.get(state, conditionedValue.eval(arg))
    def gradient = VectorTerm(withStateDo.get(_, conditionedCoefficient.value(arg)),Set(parameter))
    def argmax = StateTerm(withStateDo.get(_, arg),Set(parameter))
  }

  /**
   * Maximizing by running some message passing algorithm on the a factor graph.
   * @param term the term to maximize
   * @param algorithm applies a message passing algorithm to the message passing graph.
   */
  case class ByMessagePassing(term: Term[Double], algorithm: MPGraph => Unit = MaxProduct.run(_, 1)) extends Max {

    val ForceLinear(_, parameter, _) = term
    private val mp = MPGraphCompiler.compile(term)
    private val withStateDo = new WithStateDo(state => {
      mp.graph.weights = parameter match {
        case u:UnusedParameter => new DenseVector(0)
        case _ => state(parameter)
      }
      algorithm(mp.graph)
    })
    def eval(state: State) = withStateDo.get(state, Good(mp.currentValue()))
    def gradient = VectorTerm(withStateDo.get(_, mp.currentGradient()),Set(parameter))
    def argmax = StateTerm(withStateDo.get(_, mp.currentArgmax()),Set(parameter))
  }



}

trait Max extends Differentiable {
  def argmax: Term[State]
}


/**
 * Takes terms and returns the the gradient, if possible.
 */
object Differentiator {

  def differentiate(term: Term[Double]): Option[Term[Vector]] = {
    term match {
      case d: Differentiable => Some(d.gradient)
      case Linear(g, _, _) => Some(g)
      case Math.DoubleAdd.Applied2(arg1, arg2) =>
        for (g1 <- differentiate(arg1); g2 <- differentiate(arg2)) yield g1 + g2
      case Math.DoubleMinus.Applied2(arg1, arg2) =>
        for (g1 <- differentiate(arg1); g2 <- differentiate(arg2)) yield g1 - g2
      case _ => None
    }
  }
}

class WithStateDo(doSomething: State => Unit) {
  private var current: State = null
  def get[T](state: State, value: => T) = {
    this.synchronized {
      if (!(state eq current)) {
        doSomething(state)
        current = state
      }
      value
    }
  }
}


/**
 * An unused parameter is a vector variable that (by contract) does not appear in a term but is still listed
 * in the variables set of terms. Unused parameters allow general double terms without free variables
 * to implement the Multivariate trait.
 */
class UnusedParameter extends Variable[Vector] {
  def domain[C >: scalapplcodefest.Vector] = Constant(Vectors).asInstanceOf[Term[Set[C]]]
  override def eval(state: State) = Good(new DenseVector(0))
}