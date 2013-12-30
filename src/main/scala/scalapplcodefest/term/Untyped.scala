package scalapplcodefest.term

import scalapplcodefest.value.Fun

/**
 * Importing this object allows for untyped pattern matching and term construction.
 * @author Sebastian Riedel
 */
object Untyped {

  import scalapplcodefest._

  object FunApp {
    def apply(f: Term[Any], a: Term[Any]) = term.FunApp(f.asInstanceOf[Term[Fun[Any, Any]]], a.asInstanceOf[Term[Any]])
    def unapply(t: Term[Any]) = t match {
      case term.FunApp(f, a) => Some(f.asInstanceOf[Term[Fun[Any, Any]]], a.asInstanceOf[Term[Any]])
      case _ => None
    }
  }

  object TupleTerm2 {
    def apply(a1: Term[Any], a2: Term[Any]) = term.TupleTerm2(a1, a2)
    def unapply(t: Term[Any]) = t match {
      case term.TupleTerm2(a1, a2) => Some(a1, a2)
      case _ => None
    }
  }

  object Reduce {
    def apply(op: Term[Any], args: Term[Any]) =
      term.Reduce(op.asInstanceOf[Term[Fun[(Any, Any), Any]]], args.asInstanceOf[Term[Seq[Any]]])

    def unapply(t: Term[Any]) = t match {
      case term.Reduce(op, args) => Some(op, args)
      case _ => None
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
      case _ => t
    }
  }

}

