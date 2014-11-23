package ml.wolfe.fg20

import ml.wolfe._

import scala.collection.mutable.ArrayBuffer

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

trait Var[+T] {
  def name: String
  override def toString = if (name != "anon") name else super.toString
}
class DiscVar[+T](val dom: Seq[T], val name: String = "anon") extends Var[T]
class ContVar(val name: String = "anon") extends Var[Double]
class VectVar(val dim: Int = 0, val name: String = "anon") extends Var[FactorieVector]


class Setting(numDisc: Int, numCont: Int = 0, numVect: Int = 0) {
  var disc = Array.ofDim[Int](numDisc)
  var cont = Array.ofDim[Double](numCont)
  var vect = Array.ofDim[FactorieVector](numVect)
}

final class PartialSetting(numDisc: Int, numCont: Int = 0, numVect: Int = 0) extends Setting(numDisc, numCont, numVect) {

  var discObs = Array.ofDim[Boolean](numDisc)
  var contObs = Array.ofDim[Boolean](numCont)
  var vectObs = Array.ofDim[Boolean](numVect)

}


trait ProblemListener {
  def observationChanged(obs: State)
}


object Problem {
  def apply[Pot <: Potential](pots: Seq[Pot], obs: State = State.empty) = {
    val discVars = pots.flatMap(_.discVars).distinct
    val contVars = pots.flatMap(_.contVars).distinct
    val vectVars = pots.flatMap(_.vectVars).distinct
    new Problem(pots, discVars, contVars, vectVars, obs)
  }
}
