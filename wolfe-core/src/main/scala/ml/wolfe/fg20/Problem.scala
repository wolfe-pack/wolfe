package ml.wolfe.fg20

import ml.wolfe._

trait Var[+T]
class DiscVar[+T](val dom: Seq[T],val name:String = "anon") extends Var[T] {
  override def toString = name
}
class ContVar extends Var[Double]
class VectVar extends Var[FactorieVector]


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

trait DiscPotential extends Potential {
  def contVars = Array.empty
  def vectVars = Array.empty
}

case class Problem(pots: Seq[Potential],
                   discVars: Seq[DiscVar[Any]],
                   contVars: Seq[ContVar] = Seq.empty,
                   vectVars: Seq[VectVar] = Seq.empty,
                   observation:State = State.empty,
                   stats:Seq[Potential] = Seq.empty) {
  def vars = discVars ++ contVars ++ vectVars
}