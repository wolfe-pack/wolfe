package scalapplcodefest

/**
 * A linear model: dot product of feature and weights, added to a base measure.
 * @author Sebastian Riedel
 */
case class LinearModel(features:Term[Vector],weights:Variable[Vector],base:Term[Double] = Constant(0.0))
  extends Term[Double] with ProxyTerm[Double] {
  import TermImplicits._
  def self = (features dot weights) + base
}

object Linear {
  def unapply(term:Term[Double]):Option[(Term[Vector],Term[Vector],Term[Double])] = term match {
    case LinearModel(f,w,b) => Some(f,w,b)
    case Math.Dot.Applied2(arg1,arg2) => Some(arg1,arg2,Constant(0.0))
    case Math.DoubleAdd.Applied2(Math.Dot.Applied2(arg1,arg2),base) => Some(arg1,arg2,base)
    case Math.DoubleAdd.Applied2(base, Math.Dot.Applied2(arg1,arg2)) => Some(arg1,arg2,base)
    case _ => None
  }
}