package ml.wolfe.fg20

import ml.wolfe._

trait Var[+T] {
  def name:String
  override def toString = if (name != "anon") name else super.toString
}
class DiscVar[+T](val dom: Seq[T],val name:String = "anon") extends Var[T]
class ContVar(val name:String = "anon") extends Var[Double]
class VectVar(val dim:Int, val name:String = "anon") extends Var[FactorieVector]


trait Potential {
  def discVars: Array[DiscVar[Any]]
  def contVars: Array[ContVar]
  def vectVars: Array[VectVar]

  def isLinear:Boolean

  def score(factor: FG#Factor, weights: FactorieVector): Double
  def statsForCurrentSetting(factor: FG#Factor): FactorieVector

  def hasNoDiscVars = sys.error("This potential has no discrete variables")
  def hasNoContVars = sys.error("This potential has no continuous variables")
  def hasNoVectVars = sys.error("This potential has no vector variables")
  def isNotLinear = sys.error("This potential is not linear")

}

class Setting(numDisc:Int, numCont:Int = 0, numVect:Int = 0) {
  var disc = Array.ofDim[Int](numDisc)
  var cont = Array.ofDim[Double](numCont)
  var vect = Array.ofDim[FactorieVector](numVect)
}

class PartialSetting(numDisc:Int, numCont:Int = 0, numVect:Int = 0) {
  var disc = Array.ofDim[Int](numDisc)
  var cont = Array.ofDim[Double](numCont)
  var vect = Array.ofDim[FactorieVector](numVect)

  var discObs = Array.ofDim[Boolean](numDisc)
  var contObs = Array.ofDim[Boolean](numCont)
  var vectObs = Array.ofDim[Boolean](numVect)

}


trait Processor {
  def score(setting:Setting):Double
}

trait Potential2 extends Potential {
  type Proc <: Processor
  def processor():Proc
}

trait Statistics {
  def stats(setting:Setting):FactorieVector
}

trait ExpFamProcessor extends Statistics with Processor {
  def score(setting: Setting) = setting.vect.last dot stats(setting)
}

trait ExpFamPotential extends Potential2 {
  type Proc <: ExpFamProcessor

}

trait DiscPotential2 extends Potential2 {
  val contVars: Array[ContVar] = Array.ofDim[ContVar](0)
  val vectVars: Array[VectVar] = Array.ofDim[VectVar](0)
}





trait DiscPotential extends Potential {
  def contVars = Array.empty
  def vectVars = Array.empty
}

case class Problem(pots: Seq[Potential],
                   discVars: Seq[DiscVar[Any]] = Seq.empty,
                   contVars: Seq[ContVar] = Seq.empty,
                   vectVars: Seq[VectVar] = Seq.empty,
                   observation:State = State.empty,
                   stats:Seq[Potential] = Seq.empty) {
  def vars = discVars ++ contVars ++ vectVars
}

