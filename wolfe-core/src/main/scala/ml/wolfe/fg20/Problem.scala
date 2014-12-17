package ml.wolfe.fg20

import ml.wolfe._

import scala.collection.mutable.{ListBuffer, ArrayBuffer}

/**
 * A Problem definition, consisting of a set of variables, a sequence of potential terms that involve these
 * variables, and a partial observation to some of the variables.
 */
class Problem[+Pot <: Potential](val pots: Seq[Pot],
                                 val discVars: Seq[DiscVar[Any]] = Seq.empty,
                                 val contVars: Seq[ContVar] = Seq.empty,
                                 val vectVars: Seq[VectVar] = Seq.empty,
                                 obs: State = State.empty,
                                 val stats: Seq[Pot] = Seq.empty) {

  def vars = discVars ++ contVars ++ vectVars


  private var _observation: State = State.empty
  val listeners = new ArrayBuffer[ProblemListener]

  def observation = _observation
  def observation_=(obs: State): Unit = {
    _observation = obs
    listeners.foreach(_.observationChanged(obs))
  }

  this.observation = obs


}

/**
 * Companion object to create mo' problems.
 */
object Problem {
  def apply[Pot <: Potential](pots: Seq[Pot], obs: State = State.empty) = {
    val discVars = pots.flatMap(_.discVars).distinct
    val contVars = pots.flatMap(_.contVars).distinct
    val vectVars = pots.flatMap(_.vectVars).distinct
    new Problem(pots, discVars, contVars, vectVars, obs)
  }
}


/**
 * A variable with values of type T.
 * @tparam T the type of the values that can be assigned to this variable.
 */
trait Var[+T] {
  def name: String
  override def toString = if (name != "anon") name else super.toString
  def default:T
}
/**
 * A discrete variable.
 * @param dom the domain of discrete values.
 * @param name the name of the variable.
 * @tparam T the type of the values that can be assigned to this variable.
 */
class DiscVar[+T](val dom: Seq[T], val name: String = "anon") extends Var[T]{
  override def default: T = dom.head
}

/**
 * A continuous variable.
 * @param name the name of the variable.
 */
class ContVar(val name: String = "anon") extends Var[Double]{
  def default = 0.0
}

/**
 * A vector variable.
 * @param dim the dimension of the vector.
 * @param name the name of the variable.
 */
class VectVar(val dim: Int = 0, val name: String = "anon") extends Var[FactorieVector] {
  def default = new SparseVector(dim)
}

/**
 * A problem listener can be notified to changes to a problem (such as different observations).
 */
trait ProblemListener {
  def observationChanged(obs: State)
}


