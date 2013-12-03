package scalapplcodefest

import org.scalautils.Good

/**
 * A term with a single multi-dimensional (vector) free variable.
 * @author Sebastian Riedel
 */
trait MultiVariate extends Term[Double] {
  type At <: MultiVariateAt
  def parameter: Variable[Vector]
  def at(argument: Vector = new DenseVector(0)): At
  def variables = Set(parameter)
  def eval(state: State) = for (argument <- parameter.eval(state)) yield at(argument).value
  def default = 0.0
  def domain[C >: Double] = ???
}

/**
 * Information about the multivariate function at a given argument.
 */
trait MultiVariateAt {
  def argument: Vector
  def value: Double
}

/**
 * A function for which we can provide a (sub)gradient at a given argument.
 */
trait Differentiable extends MultiVariate {type At <: DifferentiableAt}

/**
 * Information about a differentiable function at a given argument.
 */
trait DifferentiableAt extends MultiVariateAt {
  def subGradient:  Vector
}

/**
 * A wrapper object that gives the inner term functionality as multivariate function.
 * @param parameter the variable in the term that serves as the multivariate argument.
 * @param self the term that serves as multivariate function.
 */
case class Parametrized(parameter: Var[Vector], self: Term[Double])
  extends MultiVariate with CompositeTerm[Double, Parametrized] {
  type At = ParametrizedAt
  def at(argument: Vector) = {
    val value = self.eval(SingletonState(parameter, argument)).get
    ParametrizedAt(argument, value)
  }
  def components = List(parameter, self)

  import CompositeTerm.CastCarelessly._

  def copy(args: Seq[Term[Any]]) = Parametrized(args(0), args(1))
}

/**
 * Parametrized term evaluated at a given argument.
 * @param argument the argument the function is evaluated at.
 * @param value the value of the function at this argument.
 */
case class ParametrizedAt(argument: Vector, value: Double) extends MultiVariateAt

/**
 * The Max functions maximizes over all free variables of a term other than the designated parameter of the multivariate
 * function.
 */
trait Max extends Differentiable {type At = MaxAt}

/**
 * Information about a Max function at a particular argument.
 * @param argument the argument vector.
 * @param argmax the state that maximizes the function at the argument.
 * @param subGradient the sub-gradient at the argument.
 * @param value the value at the argument.
 */
case class MaxAt(argument: Vector, argmax: State, subGradient: Vector, value: Double) extends DifferentiableAt

/**
 * Collection of different (possibly approximate) implementations of the Max function.
 */
object Max {

  import TermImplicits._

  /**
   * Maximizes the term by exhaustive search.
   * @param term the term to maximize.
   */
  case class ByBruteForce(term:Term[Double]) extends Max {
    val ForceLinear(coefficient,parameter,_) = term
    def at(argument: Vector) = {
      val injected = term | parameter -> argument
      val argmax = State.allStates(injected.variables.toList).view.maxBy(injected.eval(_).get)
      val gradient = coefficient.eval(argmax).get
      val value = injected.eval(argmax).get
      MaxAt(argument, argmax, gradient, value)
    }
  }

  case class ByMessagePassing(term:Term[Double], algorithm: MPGraph => Unit) extends Max {

    val mp = MPGraphCompiler.compile(term)
    val ForceLinear(_,parameter,_) = term

    def at(argument: Vector) = {
      mp.graph.weights = argument
      algorithm(mp.graph)
      val argmax = mp.currentArgmax()
      val gradient = mp.currentGradient()
      val value = mp.currentValue()
      MaxAt(argument, argmax, gradient, value)
    }
  }
}

/**
 * An unused parameter is a vector variable that (by contract) does not appear in a term but is still listed
 * in the terms set of variables. Unused parameters allow general double terms without free variables
 * to implement the Multivariate trait.
 */
class UnusedParameter extends Variable[Vector] {
  def domain[C >: scalapplcodefest.Vector] = Constant(Vectors).asInstanceOf[Term[Set[C]]]
  override def eval(state: State) = Good(new DenseVector(0))
}