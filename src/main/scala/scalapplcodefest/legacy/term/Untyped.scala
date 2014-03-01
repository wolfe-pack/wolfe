package scalapplcodefest.legacy.term

import scalapplcodefest.legacy.value.Fun
import scalapplcodefest.legacy._

/**
 * Importing this object allows for untyped pattern matching and term construction.
 * @author Sebastian Riedel
 */
object Untyped {

  import scalapplcodefest._

  object FunApp {
    def apply(f: Term[Any], a: Term[Any]) = term.FunApp(f.asInstanceOf[Term[Fun[Any, Any]]], a.asInstanceOf[Term[Any]])
    def unapply(t: Term[Any]) = t match {
      case scalapplcodefest.legacy.term.FunApp(f, a) => Some(f.asInstanceOf[Term[Fun[Any, Any]]], a.asInstanceOf[Term[Any]])
      case _ => None
    }
  }

  object TupleTerm2 {
    def apply(a1: Term[Any], a2: Term[Any]) = term.TupleTerm2(a1, a2)
    def unapply(t: Term[Any]) = t match {
      case scalapplcodefest.legacy.term.TupleTerm2(a1, a2) => Some(a1, a2)
      case _ => None
    }
  }

  object Reduce {
    def apply(op: Term[Any], args: Term[Any]) =
      term.Reduce(op.asInstanceOf[Term[Fun[(Any, Any), Any]]], args.asInstanceOf[Term[Seq[Any]]])

    def unapply(t: Term[Any]) = t match {
      case scalapplcodefest.legacy.term.Reduce(op, args) => Some(op, args)
      case _ => None
    }
  }

  object LambdaAbstraction {
    def apply(sig:Sig[Any], body:Term[Any]) = term.LambdaAbstraction(sig,body)

    def unapply(t:Term[Any]) = t match {
      case scalapplcodefest.legacy.term.LambdaAbstraction(s,body) => Some(s,body)
    }
  }

  def main(args: Array[String]) {
    import TermDSL._
    val b = 'b of bools
    val t = (b && b) <=> b
    val c = t match {
      case FunApp(bools.equiv, a@TupleTerm2(l, r)) =>
        FunApp(bools.and,
          TupleTerm2(
            FunApp(bools.implies, TupleTerm2(l, r)),
            FunApp(bools.implies, TupleTerm2(r, l))))
      case Reduce(bools.or, ImageSeq1(LambdaAbstraction(sig,body))) =>
        val grounded = sig.domain.value().toSeq.map(v => TermConverter.groundSig(body,sig,v))
        grounded.reduce[Term[Any]]({ case (l,r) => FunApp(bools.or,TupleTerm2(l,r))})
      case _ => t
    }

  }

}

