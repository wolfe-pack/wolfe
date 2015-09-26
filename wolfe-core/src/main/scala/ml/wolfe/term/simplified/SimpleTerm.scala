package ml.wolfe.term.simplified

import scala.language.implicitConversions

object SimpleTerm {

  import Wolfe._

  def main(args: Array[String]) {
    implicit val domains = new Domains
    val Ints = RangeDom(0 until 3)
    val Seqs = SeqDom(Doubles, 2, 2)
    val Pairs = Tuple2Dom(Ints, Doubles)
    //implicit val domains = Domains ...
    //val Seqs1 = Seqs(...,...)
    //val s = Seqs1.Var //remembers binding
    val s = Seqs.Var
    val i = Ints.Var
    val t = Pairs.Var
    import BaseEval._
    println(s(i + i).eval(i := 1, s := Seq(1.0, 2.0, 3.0)))

  }

  def dom[T](binding: Map[Var[Any], Dom[Any]])(term: STerm[Any]): Dom[Any] = term match {
    case v: Var[_] => binding(v)
    case SeqApply(s, i) => dom(binding)(s) match {
      case SeqDom(e, _, _) => e
    }
    case Plus(a1, a2) => dom(binding)(a1) -> dom(binding)(a2) match {
      case (RangeDom(r1), RangeDom(r2)) => RangeDom(Range(r1.start + r2.start, r1.end + r2.end))
    }

  }

}

