package scalapplcodefest.term

import scalapplcodefest.value.Fun

/**
 * @author Sebastian Riedel
 */
object Untyped {

  import scalapplcodefest._

  object FunApp {
    def apply(f:Term[Any],a:Term[Any]) = term.FunApp(f.asInstanceOf[Term[Fun[Any,Any]]], a.asInstanceOf[Term[Any]])
    def unapply(t:Term[Any]) = t match {
      case term.FunApp(f,a) => Some(f.asInstanceOf[Term[Fun[Any,Any]]],a.asInstanceOf[Term[Any]])
      case _ => None
    }
  }

  object TupleTerm2 {
    def apply(a1:Term[Any],a2:Term[Any]) = term.TupleTerm2(a1,a2)
    def unapply(t:Term[Any]) = t match {
      case term.TupleTerm2(a1,a2) => Some(a1,a2)
      case _ => None
    }
  }

  def main(args: Array[String]) {
    import TermDSL._
    val b = 'b of bools
    val t = (b && b) <=> b
    val c = t match {
      case FunApp(bools.equiv, a@TupleTerm2(FunApp(bools.and, TupleTerm2(t1,t2)),t3)) =>
        FunApp(bools.and, TupleTerm2(FunApp(bools.implies, TupleTerm2(a,t3)),t2))
      case _ => ???
    }
  }

}

