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

  import Math._
  import TermImplicits._

  def unapply(term:Term[Double]):Option[(Term[Vector],Term[Vector],Term[Double])] = term match {
    case LinearModel(f,w,b) => Some(f,w,b)
    case DoubleAdd.Reduced(SeqTerm(args)) =>
      val featWeight = args collectFirst {
        case t@Dot.Applied2(f,v@Var(_,_)) => t -> (f,v)
        case t@Dot.Applied2(v@Var(_,_),f) => t -> (f,v)
      }
      featWeight match {
        case Some((t,(f,v))) => Some(f,v,dsum(SeqTerm(args.filter( _ != t))))
        case _ => None
      }
    case Dot.Applied2(arg1,arg2) => Some(arg1,arg2,Constant(0.0))
    case DoubleAdd.Applied2(Math.Dot.Applied2(arg1,arg2),base) => Some(arg1,arg2,base)
    case DoubleAdd.Applied2(base, Math.Dot.Applied2(arg1,arg2)) => Some(arg1,arg2,base)
    case _ => None
  }
}