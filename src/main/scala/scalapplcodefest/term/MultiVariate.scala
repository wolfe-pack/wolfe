package scalapplcodefest.term

import org.scalautils.Good
import scalapplcodefest.TermConverter._
import scalapplcodefest.value.{Fun, Vectors}
import scalapplcodefest._
import scala.Some
import TermDSL._

object MultiVariate {
  def unapply(term: Term[Double]): Option[Variable[Vector]] = term match {
    case Conditioned(MultiVariate(p), c) if !c.domain(p) => Some(p)
    case m: MultiVariate => Some(m.parameter)
    case Linear(_, p, _) => Some(p)
    case doubles.add.Applied2(MultiVariate(p1), MultiVariate(p2)) if p1 == p2 => Some(p1)
    case doubles.minus.Applied2(MultiVariate(p1), MultiVariate(p2)) if p1 == p2 => Some(p1)
    case _ => Some(new UnusedParameter)
  }
}

trait MultiVariate extends Term[Double] {
  def parameter: Variable[Vector]
}

trait MultiVariateHelper extends MultiVariate with DoubleTerm {
  def variables = Set(parameter)
}

object Differentiable {
  def unapply(term: Term[Double]): Option[(Variable[Vector], Term[Vector])] = term match {
    case d: Differentiable => Some(d.parameter, d.gradient)
    case MultiVariate(p) => Differentiator.differentiate(p, term).map(g => (p, g))
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
  case class ByBruteForce[T](term: LambdaAbstraction[T,Double]) extends Max with Composite1[Fun[T,Double], Double] with DoubleTerm {
    val ForceLinear(coefficient, parameter, _) = term.body
    private var arg: State = _
    private var conditionedValue: Term[Double] = _
    private var conditionedCoefficient: Term[Vector] = _
    private val withStateDo = new WithStateDo(state => {
      val argument = state(parameter)
      conditionedValue = term.body | parameter -> argument
      conditionedCoefficient = coefficient | parameter -> argument
      arg = State.allStates(term.sig.variables.toList).view.maxBy(conditionedValue.eval(_).get)
    })
    def eval(state: State) = withStateDo.get(state, conditionedValue.eval(arg))
    def gradient = VectorTerm(withStateDo.get(_, conditionedCoefficient.value(arg)), Set(parameter))
    def argmax = StateTerm(withStateDo.get(_, arg), Set(parameter))
    def copy(t1: Term[Fun[T,Double]]) = ByBruteForce(t1.asInstanceOf[LambdaAbstraction[T,Double]])
    def components = term
  }



  /**
   * Maximizing by running some message passing algorithm on the a factor graph.
   * @param term the term to maximize
   * @param algorithm applies a message passing algorithm to the message passing graph.
   */
  case class ByMessagePassing[T](term: LambdaAbstraction[T,Double], algorithm: MPGraph => Unit = MaxProduct.apply(_, 1)) extends Max with MultiVariateHelper{

    val normalized = pushDownConditions(term.body)
    val ForceLinear(_, parameter, _) = normalized

    private val mp = MPGraphCompiler.compile(term.sig, normalized)

    private val withStateDo = new WithStateDo(state => {
      mp.graph.weights = parameter.value(state)
      //println(mp.graph.toVerboseString(mp.printer()))
      algorithm(mp.graph)
    })
    def eval(state: State) = withStateDo.get(state, Good(mp.currentValue()))
    def gradient = VectorTerm(withStateDo.get(_, mp.currentGradient()), Set(parameter))
    def argmax = StateTerm(withStateDo.get(_, mp.currentArgmax()), Set(parameter))
  }

}

/**
 * A Term that corresponds to a maximization.
 */
trait Max extends Differentiable {
  def argmax: Term[State]
}

trait Max2[T] extends Term[Double] {
  def argmax: Term[T]
}



/**
 * Takes terms and returns the the gradient, if possible.
 */
object Differentiator {

  def differentiate(param: Variable[Vector], term: Term[Double]): Option[Term[Vector]] = {
    term match {
      case d: Differentiable => Some(d.gradient)
      case Conditioned(t, c) => c.domain(param) match {
        case true => Some(Constant(new SparseVector(0)))
        case false => differentiate(param, t).map(v => Conditioned(v, c))
      }
      case Linear(g, p, _) if p == param => Some(g)
      case doubles.add.Applied2(arg1, arg2) =>
        for (g1 <- differentiate(param, arg1); g2 <- differentiate(param, arg2)) yield g1 + g2
      case doubles.minus.Applied2(arg1, arg2) =>
        for (g1 <- differentiate(param, arg1); g2 <- differentiate(param, arg2)) yield g1 - g2
      case _ => None
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

/**
 * Something that has a score term.
 */
trait Scored {
  def score: Term[Double]
}

trait MaxHint extends CompilerHint {
  type ArgmaxValue[T] = (Term[T], Term[Double])
  type ArgmaxValueGradient[T] = (Term[T], Term[Double], Term[Vector])
  // should have no free variables
  def withoutParam[T](sig: Sig[T], term: Term[Double]): Max2[T]
  //should have one free variable: param
  def withParam[T](sig: Sig[T], param: Variable[Vector], term: Term[Double]): Max2[T] with Differentiable
}

case class MessagePassingHint(algorithm: MPGraph => Unit = MaxProduct.apply(_, 1)) extends MaxHint {
  def withoutParam[T](sig: Sig[T], term: Term[Double]) = {
    require(term.variables.forall(sig.variables(_)),
      s"Unbound variable ${term.variables.find(!sig.variables(_))}")
    val mpg = MPGraphCompilerExperimental.compile(sig, term)
    val withStateDo = new WithStateDo(state => algorithm(mpg.graph))
    val argmaxAt = Term(withStateDo.get(_, mpg.currentArgmax()), Set.empty, sig.default)
    val valueAt = Term(withStateDo.get(_, mpg.currentValue()), Set.empty, 0.0)
    new ProxyTerm[Double] with Max2[T] {
      def argmax = argmaxAt
      def self = valueAt
    }
  }

  def withParam[T](sig: Sig[T], param: Variable[scalapplcodefest.Vector], term: Term[Double]) = {
    require((term.variables - param).forall(sig.variables(_)),
      s"Unbound variable ${(term.variables - param).find(!sig.variables(_))}")
    val mpg = MPGraphCompilerExperimental.compile(sig, term, Some(param)) //pass signature
    val withStateDo = new WithStateDo(state => {mpg.graph.weights = state(param); algorithm(mpg.graph)})
    val argmaxAt = Term(withStateDo.get(_, mpg.currentArgmax()), Set(param), sig.default)
    val valueAt = Term(withStateDo.get(_, mpg.currentValue()), Set(param), 0.0)
    val gradientAt = Term(withStateDo.get(_, mpg.currentGradient()), Set(param), Vectors.Zero)
    new ProxyTerm[Double] with Max2[T] with Differentiable {
      def gradient = gradientAt
      def parameter = param
      def argmax = argmaxAt
      def self = valueAt
    }
  }
}




