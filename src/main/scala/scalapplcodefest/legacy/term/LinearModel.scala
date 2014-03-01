package scalapplcodefest.legacy.term

import scalapplcodefest._
import scala.Some


object Linear {

  import legacy.TermDSL._

  def unapply(term:Term[Double]):Option[(Term[Vector],Variable[Vector],Term[Double])] = term match {
    case doubles.add.Reduced(SeqTerm(args)) =>
      val featWeight = args collectFirst {
        case t@vectors.dot.Applied2(f,v@Var(_,_)) => t -> (f,v)
        case t@vectors.dot.Applied2(v@Var(_,_),f) => t -> (f,v)
      }
      featWeight match {
        case Some((t,(f,v))) => Some(f,v,doubles.sumSeq(SeqTerm(args.filter( _ != t))))
        case _ => None
      }
    case vectors.dot.Applied2(f,w@Var(_,_)) => Some(f,w,Constant(0.0))
    case doubles.add.Applied2(vectors.dot.Applied2(f,w@Var(_,_)),base) => Some(f,w,base)
    case doubles.add.Applied2(base, vectors.dot.Applied2(f,w@Var(_,_))) => Some(f,w,base)
    case _ => None
  }
}

/**
 * Forces a double term into linear form: either the term has a `coeff * param + base` structure,
 * or a new unused variable x is introduced, and the term becomes `0 * x + term`.
 */
object ForceLinear {
  def unapply(term:Term[Double]):Option[(Term[Vector],Variable[Vector],Term[Double])] = term match {
    case Linear(c,p,b) => Some(c,p,b)
    case _ => Some(Constant(new SparseVector(0)),new UnusedParameter, term)
  }
}