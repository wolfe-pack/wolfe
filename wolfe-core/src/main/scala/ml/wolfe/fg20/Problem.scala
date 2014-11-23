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


trait Potential {
  def discVars: Array[DiscVar[Any]]
  def contVars: Array[ContVar]
  def vectVars: Array[VectVar]

  def score(factor: FG#Factor, weights: FactorieVector): Double
  def statsForCurrentSetting(factor: FG#Factor): FactorieVector

  def hasNoDiscVars = sys.error("This potential has no discrete variables")
  def hasNoContVars = sys.error("This potential has no continuous variables")
  def hasNoVectVars = sys.error("This potential has no vector variables")
  def isNotLinear = sys.error("This potential is not linear")

  def isLinear:Boolean

}

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

trait Potential2 extends Potential {
  type Proc <: Processor
  def processor(): Proc

  def createSetting() = new Setting(discVars.size,contVars.size,vectVars.size)

}

trait StatelessProcessor[This <: StatelessProcessor[This]] extends Potential2 with Processor {
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


trait ExpFamPotential extends Potential2 {
  type Proc <: ExpFamProcessor

  def weights(setting: Setting) = setting.vect(setting.vect.length - 1)

}

trait DiscPotential2 extends Potential2 {
  val contVars: Array[ContVar] = Array.ofDim[ContVar](0)
  val vectVars: Array[VectVar] = Array.ofDim[VectVar](0)
}


trait DiscPotential extends Potential {
  def contVars = Array.empty
  def vectVars = Array.empty
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
