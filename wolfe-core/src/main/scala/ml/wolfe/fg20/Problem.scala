package ml.wolfe.fg20

import ml.wolfe._

trait Var[+T]
class DiscVar[+T](val dom: Seq[T]) extends Var[T]
class ContVar extends Var[Double]
class VectVar extends Var[FactorieVector]

trait Potential {
  def discVars: Array[DiscVar[Any]]
  def contVars: Array[ContVar]
  def vectVars: Array[VectVar]
}

trait DiscPotential extends Potential {
  def contVars = Array.empty
  def vectVars = Array.empty
}

class ImpliesPotential(premise:DiscVar[Boolean],consequent:DiscVar[Boolean]) extends DiscPotential {
  def discVars = Array(premise,consequent)
}

case class Problem(discVars: Seq[DiscVar[Any]], contVars: Seq[ContVar], vectVars: Seq[VectVar], pots: Seq[Potential])