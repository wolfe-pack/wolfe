package ml.wolfe.legacy.term

import org.scalautils.{Bad, Good}
import ml.wolfe.legacy.value._
import ml.wolfe.legacy.{TermDSL, UtilOld, SetUtil}

/**
 * @author Sebastian Riedel
 */
case class TupleTerm2[A1, A2](a1: Term[A1], a2: Term[A2])
  extends Composite2[A1,A2,(A1, A2)] {
  import SetCastHelper._
  def components = (a1,a2)
  //def copy(args: Seq[Term[Any]]) = TupleTerm2(args(0).asInstanceOf[Term[A1]],args(1).asInstanceOf[Term[A2]])
  def eval(state: State) = for (b1 <- a1.eval(state); b2 <- a2.eval(state)) yield (b1, b2)
  def default = (a1.default, a2.default)
  def domain[C >: (A1, A2)] = CartesianProductTerm2(a1.domain,a2.domain).as[C]
  def copy(t1: Term[A1], t2: Term[A2]) = TupleTerm2(t1,t2)
  override def toString = s"($a1, $a2)"
}

object ProductTerm {
  def unapply(term:Term[Any]):Option[Seq[Term[Any]]] = term match {
    case TupleTerm2(a1,a2) => Some(Seq(a1.domain,a2.domain))
    case TupleTerm3(a1,a2,a3) => Some(Seq(a1.domain,a2.domain,a3.domain))
    case _ => None
  }
}

case class TupleTerm3[A1, A2, A3](a1: Term[A1], a2: Term[A2], a3: Term[A3])
  extends Composite3[A1,A2,A3,(A1, A2, A3)] {
  import SetCastHelper._
  def eval(state: State) = for (b1 <- a1.eval(state);
                                b2 <- a2.eval(state);
                                b3 <- a3.eval(state)) yield (b1, b2, b3)
  def default = (a1.default, a2.default, a3.default)
  def domain[C >: (A1, A2, A3)] = CartesianProductTerm3(a1.domain, a2.domain, a3.domain).as[C]
  def components = (a1,a2,a3)
  def copy(t1: Term[A1], t2: Term[A2], t3: Term[A3]) = TupleTerm3(t1,t2,t3)
}

case class ArgOf[P <: Product, A](product:Term[P],index:Term[Int]) extends Term[A] with Composite2[P,Int,A] {
  def domain[C >: A] = TermDSL.all[C]
  def default = product.default.productElement(index.default).asInstanceOf[A]
  def eval(state: State) = for (p <- product.eval(state); i <- index.eval(state)) yield p.productElement(i).asInstanceOf[A]
  def components = (product,index)
  def copy(t1: Term[P], t2: Term[Int]) = ArgOf(t1,t2)
}

case class CartesianProductTerm2[A1,A2](a1:Term[Set[A1]],a2:Term[Set[A2]]) extends Term[Set[(A1,A2)]] {
  def eval(state: State) =
    for (b1 <- a1.eval(state);
         b2 <- a2.eval(state)) yield CartesianProduct2(b1,b2)
  def variables = a1.variables ++ a2.variables
  def default = Set((a1.default.head,a2.default.head))
  def domain[C >: Set[(A1, A2)]] = TermDSL.all[C]
}

case class CartesianProductTerm3[A1,A2,A3](a1:Term[Set[A1]],a2:Term[Set[A2]],a3:Term[Set[A3]]) extends Term[Set[(A1,A2,A3)]] {
  def eval(state: State) =
    for (b1 <- a1.eval(state);
         b2 <- a2.eval(state);
         b3 <- a3.eval(state)) yield CartesianProduct3(b1,b2,b3)
  def variables = a1.variables ++ a2.variables ++ a3.variables
  def default = Set((a1.default.head,a2.default.head,a3.default.head))
  def domain[C >: Set[(A1, A2, A3)]] = TermDSL.all[C]
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
case class SeqTerm[T](seq:Seq[Term[T]]) extends Term[Seq[T]] with Composite[Seq[T]] {
  def eval(state: State) = {
    val result = seq.map(_.eval(state))
    if (result.forall(_.isGood)) Good(result.map(_.get)) else Bad(result.find(_.isBad).get.swap.get)
  }
  def domain[C >: Seq[T]] = Constant(new AllOfType[C])
  def default = seq.map(_.default)
  override def toString = seq.mkString("[",",","]")
  def componentSeq = seq
  def copySeq(args: Seq[Term[Any]]) = SeqTerm(args).asInstanceOf[Term[Seq[T]]]
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

