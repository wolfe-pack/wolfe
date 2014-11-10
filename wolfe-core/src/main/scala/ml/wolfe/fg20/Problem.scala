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

  def hasNoDiscVars = sys.error("This potential has no discrete variables")
  def hasNoContVars = sys.error("This potential has no continuous variables")
  def hasNoVectVars = sys.error("This potential has no vector variables")
  def isNotLinear = sys.error("This potential is not linear")


}

trait DiscPotential extends Potential {
  def contVars = Array.empty
  def vectVars = Array.empty
}



class ImpliesPotential(premise:DiscVar[Boolean],consequent:DiscVar[Boolean]) extends DiscPotential {
  def discVars = Array(premise,consequent)


//  def value(factor:Factor[_, _, _, _]): Double = {
//    0.0
//  }
  def isLinear = false
}



case class Problem(pots: Seq[Potential], discVars: Seq[DiscVar[Any]],
                   contVars: Seq[ContVar] = Seq.empty, vectVars: Seq[VectVar] = Seq.empty) {
  def vars = discVars ++ contVars ++ vectVars
}