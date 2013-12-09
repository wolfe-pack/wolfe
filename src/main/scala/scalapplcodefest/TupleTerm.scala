package scalapplcodefest

import org.scalautils.{Bad, Good}

/**
 * @author Sebastian Riedel
 */
case class TupleTerm2[+A1, +A2](a1: Term[A1], a2: Term[A2])
  extends Term[(A1, A2)] {
  import SetCastHelper._
  def variables = SetUtil.SetUnion(List(a1.variables,a2.variables))
  def components = List(a1,a2)
  //def copy(args: Seq[Term[Any]]) = TupleTerm2(args(0).asInstanceOf[Term[A1]],args(1).asInstanceOf[Term[A2]])
  def eval(state: State) = for (b1 <- a1.eval(state); b2 <- a2.eval(state)) yield (b1, b2)
  def default = (a1.default, a2.default)
  def domain[C >: (A1, A2)] = CartesianProductTerm2(a1.domain,a2.domain).as[C]
  override def toString = s"$a1, $a2"
}

case class TupleTerm3[A1, A2, A3](a1: Term[A1], a2: Term[A2], a3: Term[A3])
  extends Term[(A1, A2, A3)] {
  import SetCastHelper._
  def variables = a1.variables ++ a2.variables ++ a3.variables
  def eval(state: State) = for (b1 <- a1.eval(state);
                                b2 <- a2.eval(state);
                                b3 <- a3.eval(state)) yield (b1, b2, b3)
  def default = (a1.default, a2.default, a3.default)
  def domain[C >: (A1, A2, A3)] = CartesianProductTerm3(a1.domain, a2.domain, a3.domain).as[C]
}

case class Arg[P <:Product, A](dom:Set[P], range:Set[A], arg:Int) extends Fun[P,A] {
  def funCandidateDom = dom
  def funRange = range
  def isDefinedAt(x: P) = dom(x)
  def apply(x: P) = x.productElement(arg).asInstanceOf[A]
}

case object ArgNew extends PartialFunction[(Product,Int),Any] {
  self =>
  def isDefinedAt(x: (Product, Int)) = x._2 < x._1.productArity
  def apply(v1: (Product, Int)) = v1._1.productElement(v1._2)

  case object Applied2 {
    def unapply(term:Term[Any]) = term match {
      case FunApp(DynFunTerm(f,dom,range),TupleTerm2(tuple,index)) if f == self =>
        Some(dom.asInstanceOf[Term[Set[(Product,Int)]]], range, tuple.asInstanceOf[Term[Product]], index.asInstanceOf[Term[Int]])
      case _ => None
    }
  }
}


case class ArgTerm[P <:Product, A](dom:Term[Set[P]], range:Term[Set[A]], arg:Term[Int]) extends FunTerm[P,A] {
  def funCandidateDom = dom
  def funRange = range
  def domain[C >: Fun[P, A]] = Constant(new AllOfType[C])
  def variables = dom.variables ++ range.variables ++ arg.variables
  def default = Arg(dom.default,range.default,arg.default)
  def eval(state: State) = for (d <- dom.eval(state); r <- range.eval(state); a <- arg.eval(state)) yield Arg(d,r,a)
}

case class CartesianProduct2[A1,A2](d1:Set[A1],d2:Set[A2]) extends SetValue[(A1,A2)] {
  def contains(elem: (A1, A2)) = d1(elem._1) && d2(elem._2)
  def iterator = for (v1 <- d1.iterator; v2 <- d2.iterator) yield (v1,v2)
}
case class CartesianProduct3[A1,A2,A3](d1:Set[A1],d2:Set[A2],d3:Set[A3]) extends SetValue[(A1,A2,A3)] {
  def contains(elem: (A1, A2, A3)) = d1(elem._1) && d2(elem._2) && d3(elem._3)
  def iterator = for (v1 <- d1.iterator; v2 <- d2.iterator; v3 <- d3.iterator) yield (v1,v2,v3)
}

//abstract class CartesianOperator2[A1,A2] extends Operator[(Set[A1],Set[A2]),Set[(A1,A2)]] {
//  def funCandidateDom = ???
//  def funRange = ???
//  def apply(v1: (Set[A1], Set[A2])) = CartesianProduct2(v1._1,v1._2)
//}

case class CartesianProductTerm2[A1,A2](a1:Term[Set[A1]],a2:Term[Set[A2]]) extends Term[Set[(A1,A2)]] {
  def eval(state: State) =
    for (b1 <- a1.eval(state);
         b2 <- a2.eval(state)) yield CartesianProduct2(b1,b2)
  def variables = a1.variables ++ a2.variables
  def default = Set((a1.default.head,a2.default.head))
  def domain[C >: Set[(A1, A2)]] = Constant(Util.setToBeImplementedLater)
}

case class CartesianProductTerm3[A1,A2,A3](a1:Term[Set[A1]],a2:Term[Set[A2]],a3:Term[Set[A3]]) extends Term[Set[(A1,A2,A3)]] {
  def eval(state: State) =
    for (b1 <- a1.eval(state);
         b2 <- a2.eval(state);
         b3 <- a3.eval(state)) yield CartesianProduct3(b1,b2,b3)
  def variables = a1.variables ++ a2.variables
  def default = Set((a1.default.head,a2.default.head,a3.default.head))
  def domain[C >: Set[(A1, A2, A3)]] = Constant(Util.setToBeImplementedLater)
}

object Wrapped3 {
  def unroll[A1,A2,A3](term:TupleTerm2[(A1,A2),A3]):Option[TupleTerm3[A1,A2,A3]] = term match {
    case TupleTerm2(TupleTerm2(a1,a2),a3) => Some(TupleTerm3(a1,a2,a3))
    case _ => None
  }
}

/**
 * A term that evaluates to a sequence of values based on the given argument terms. The term value
 * is undefined if any of the argument term values are undefined.
 * @param seq the sequence of terms that is turned into a sequence of values.
 * @tparam T type of values in the sequence.
 */
case class SeqTerm[T](seq:Seq[Term[T]]) extends Term[Seq[T]] {
  def eval(state: State) = {
    val result = seq.map(_.eval(state))
    if (result.forall(_.isGood)) Good(result.map(_.get)) else Bad(result.find(_.isBad).get.swap.get)
  }
  def variables = SetUtil.SetUnion(seq.toList.map(_.variables))
  def domain[C >: Seq[T]] = Constant(new AllOfType[C])
  def default = seq.map(_.default)
  override def toString = seq.mkString("[",",","]")
}

/**
 * Helper to identify sequence of terms that are all variables
 */
object VarSeq {
  def unapply[T](term:Term[Seq[T]]):Option[Seq[Variable[T]]] = term match {
    case SeqTerm(args) if args.forall(_.isInstanceOf[Variable[_]]) => Some(args.map(_.asInstanceOf[Variable[T]]))
    case _ => None
  }
}


case object MapIterable extends PartialFunction[(Iterable[Any],Fun[Any,Any]),Iterable[Any]] {
  def apply(v1: (Iterable[Any], Fun[Any, Any])) = v1 match {case (s,f) => s.map(f)}
  def isDefinedAt(x: (Iterable[Any], Fun[Any, Any])) = true
}

case object CollectIterable extends PartialFunction[(Iterable[Any],Fun[Any,Any]),Iterable[Any]] {
  def apply(v1: (Iterable[Any], Fun[Any, Any])) = v1 match {case (s,f) => s.collect(f)}
  def isDefinedAt(x: (Iterable[Any], Fun[Any, Any])) = true
}

case object FilterIterable extends PartialFunction[(Iterable[Any],Fun[Any,Boolean]),Iterable[Any]] {
  def apply(v1: (Iterable[Any], Fun[Any, Boolean])) = v1 match {case (s,f) => s.filter(f)}
  def isDefinedAt(x: (Iterable[Any], Fun[Any, Boolean])) = true
}

case object IsDefined extends PartialFunction[(PartialFunction[Any,Any],Any),Boolean] {
  def isDefinedAt(x: (PartialFunction[Any, Any], Any)) = true
  def apply(x: (PartialFunction[Any, Any], Any)) = x match {case (f,a) => f.isDefinedAt(a)}
}

case object IntDivide extends PartialFunction[(Int,Int),Int] {
  def isDefinedAt(x: (Int, Int)) = x._2 != 0
  def apply(x:(Int,Int)) = x._1 / x._2
}