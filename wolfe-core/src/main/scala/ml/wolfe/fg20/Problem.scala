package ml.wolfe.fg20

import ml.wolfe._

import scala.collection.mutable.ArrayBuffer

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

class PartialSetting(numDisc: Int, numCont: Int = 0, numVect: Int = 0) extends Setting(numDisc, numCont, numVect) {

  var discObs = Array.ofDim[Boolean](numDisc)
  var contObs = Array.ofDim[Boolean](numCont)
  var vectObs = Array.ofDim[Boolean](numVect)

}

trait Processor {
  def score(setting: Setting): Double
}

trait Potential {

  def discVars: Array[DiscVar[Any]]
  def contVars: Array[ContVar]
  def vectVars: Array[VectVar]

  type Proc <: Processor
  def processor(): Proc

  def createSetting() = new Setting(discVars.size,contVars.size,vectVars.size)

}

trait StatelessProcessor[This <: StatelessProcessor[This]] extends Potential with Processor {
  this: This =>
  type Proc = This
  def processor():This = this
}

trait Statistics {
  def stats(setting: Setting): FactorieVector
}

trait ExpFamProcessor extends Statistics with Processor {
  def score(setting: Setting) = setting.vect(setting.vect.length - 1) dot stats(setting)
}


trait ExpFamPotential extends Potential {
  type Proc <: ExpFamProcessor

  def weights(setting: Setting) = setting.vect(setting.vect.length - 1)

}

trait DiscPotential extends Potential {
  val contVars: Array[ContVar] = Array.ofDim[ContVar](0)
  val vectVars: Array[VectVar] = Array.ofDim[VectVar](0)
}

trait ContPotential extends Potential {
  val discVars: Array[DiscVar[Any]] = Array.ofDim[DiscVar[Any]](0)
  val vectVars: Array[VectVar] = Array.ofDim[VectVar](0)
}

trait VectPotential extends Potential {
  val discVars: Array[DiscVar[Any]] = Array.ofDim[DiscVar[Any]](0)
  val contVars: Array[ContVar] = Array.ofDim[ContVar](0)
}






trait ProblemListener {
  def observationChanged(obs:State)
}

class Problem(val pots: Seq[Potential],
              val discVars: Seq[DiscVar[Any]] = Seq.empty,
              val contVars: Seq[ContVar] = Seq.empty,
              val vectVars: Seq[VectVar] = Seq.empty,
              obs:State = State.empty,
              val stats: Seq[Potential] = Seq.empty) {
  def vars = discVars ++ contVars ++ vectVars


  private var _observation: State = State.empty
  val listeners = new ArrayBuffer[ProblemListener]

  def observation = _observation
  def observation_=(obs:State): Unit = {
    _observation = obs
    listeners.foreach(_.observationChanged(obs))
  }

  this.observation = obs


}

object Problem {
  def apply(pots:Seq[Potential], obs:State = State.empty) = {
    val discVars = pots.flatMap(_.discVars).distinct
    val contVars = pots.flatMap(_.contVars).distinct
    val vectVars = pots.flatMap(_.vectVars).distinct
    new Problem(pots, discVars, contVars,vectVars, obs)
  }
}
