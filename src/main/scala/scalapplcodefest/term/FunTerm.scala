package scalapplcodefest.term

import org.scalautils.{Bad, Good}
import scalapplcodefest.value.{Fun, AllOfType, SetValue}
import scalapplcodefest._

/**
 * FunTerms evaluate to partial functions. The function candidate domain where the function is defined for
 * may depend on the state, and is hence a term itself.
 * The general contract for partial functions with the given function candidate domain and target set are as follows:
 * the function is undefined for all values outside the candidate domain, and every result is part of the range. Note that
 * the function may also be undefined for some values inside the candidate domain.
 * @tparam A type of arguments to function.
 * @tparam B type of return values of function.
 */
trait FunTerm[A, B] extends Term[Fun[A, B]] {

  import TermDSL._

  def funCandidateDom: Term[Set[A]]
  def funRange: Term[Set[B]]
  def funDom = funCandidateDom.filteredBy(this.isDefined)
}

/**
 * Helper object to build FunTerms
 */
object FunTerm {

  import TermDSL._

  def unapply[A, B](term: Term[Fun[A, B]]): Option[(Term[Set[A]], Term[Set[B]])] = term match {
    case f: FunTerm[_, _] => Some(f.funCandidateDom.asInstanceOf[Term[Set[A]]], f.funRange.asInstanceOf[Term[Set[B]]])
    case Constant(f) => Some(Constant(f.funCandidateDom), Constant(f.funRange))
    case _ => Some(all[A], all[B])
  }

  /**
   * Create a function term from a partial function
   * @param f the partial function to wrap the term around.
   * @tparam A argument type.
   * @tparam B return type.
   * @return a function term that evaluates to the given partial function.
   */
  def fromPartial[A, B](f: PartialFunction[A, B]) = RestrictedFun[A, B](f)


}

/**
 * A constant function with dynamic domain and range.
 * @param fun the constant function.
 * @param funCandidateDom the domain term. Depending on its value the domain of the function can be limited.
 * @param funRange the range term. Depending on its value the range of the function can be limited. This means that
 *                 for predicates some arguments values can become undefined.
 * @tparam A type of arguments to function.
 * @tparam B type of return values of function.
 */
case class DynFunTerm[A, B](fun: PartialFunction[A, B], funCandidateDom: Term[Set[A]], funRange: Term[Set[B]]) extends FunTerm[A, B] {
  def variables = funCandidateDom.variables ++ funRange.variables
  def default = Fun(fun, funCandidateDom.default, funRange.default)
  def domain[C >: Fun[A, B]] = Constant(new AllOfType[C])
  def eval(state: State) = for (d <- funCandidateDom.eval(state); r <- funRange.eval(state)) yield Fun(fun, d, r)
}

case class RestrictedFun[A, B](fun: AnyFunction,
                               funCandidateDom: Term[Set[A]] = Constant(new AllOfType[A]),
                               funRange: Term[Set[B]] = Constant(new AllOfType[B]))
  extends FunTerm[A, B] with Composite2[Set[A],Set[B],Fun[A,B]] {

  def cast = fun.asInstanceOf[PartialFunction[A, B]]
  def variables = funCandidateDom.variables ++ funRange.variables
  def default = Fun(cast, funCandidateDom.default, funRange.default)
  def domain[C >: Fun[A, B]] = Constant(new AllOfType[C])
  def eval(state: State) = for (d <- funCandidateDom.eval(state); r <- funRange.eval(state)) yield Fun(cast, d, r)
  def components = (funCandidateDom,funRange)
  def copy(t1: Term[Set[A]], t2: Term[Set[B]]) = RestrictedFun(fun,t1,t2)
}

/**
 * Application of a function to an argument
 * @param function the function to apply
 * @param arg the argument to apply the function to
 * @tparam A argument type of function
 * @tparam B return type of function
 */
case class FunApp[A, B](function: Term[Fun[A, B]], arg: Term[A]) extends Term[B] with Composite2[Fun[A,B],A,B]{
  val FunTerm(funCandidateDom, funRange) = function
  def eval(state: State) =
    for (f <- function.eval(state);
         a <- arg.eval(state);
         v <- f.lift(a).map(Good(_)).getOrElse(Bad(FunctionNotDefinedAt(this, state)))) yield v
  def variables = function match {
    case p@Predicate(n, d, r) => PartialGroundAtoms(p, arg)
    case _ => SetUtil.SetUnion(List(function.variables, arg.variables))
  }
  def default = function.default(funCandidateDom.default.head)
  def domain[C >: B] = Image(function, arg.domain).asInstanceOf[Term[Set[C]]] // TermDSL.all[C]//
  //replace by function.funDomain collectedBy function?
  //could also be function.funRange
  override def toString = s"$function($arg)"
  def components = (function,arg)
  def copy(t1: Term[Fun[A, B]], t2: Term[A]) = FunApp(t1,t2)
}

case object BinaryFunApp {
  def unapply[T](term: FunApp[(T, T), T]) = term match {
    case FunApp(f, a) => Some((f, a))
    case _ => None
  }
}




/**
 * The Image of a function term is the set of return values we get by applying
 * the function to all elements of its domain.
 * @param fun the function to get the image for
 * @tparam A argument type of function.
 * @tparam B return type of function.
 */
case class Image[A, B](fun: Term[Fun[A, B]], dom: Term[Set[A]]) extends Term[Set[B]] {
  def eval(state: State) = for (f <- fun.eval(state); d <- dom.eval(state)) yield SetUtil.SetMap(d, f)
  def variables = SetUtil.SetUnion(List(fun.variables, dom.variables))
  def domain[C >: Set[B]] = Constant(Util.setToBeImplementedLater[C])
  def default = fun.default.funRange
}

/**
 * The ImageSeq of a function term is a sequence of return values we get by applying
 * the function to all elements of its domain (in some undefined order). Compared to the image
 * of a function, here return values can be repeated.
 * @param fun the function to get the image for
 * @tparam A argument type of function.
 * @tparam B return type of function.
 */
case class ImageSeq1[A, B](fun: Term[Fun[A, B]]) extends Term[Seq[B]] {
  val FunTerm(funCandidateDom, _) = fun
  def eval(state: State) = for (f <- fun.eval(state); d <- funCandidateDom.eval(state)) yield d.view.toSeq.map(f)
  def variables = fun.variables
  def domain[C >: Seq[B]] = Constant(new AllOfType[C])
  def default = fun.default.funRange.toSeq
}

/**
 * The sequence of terms we get by iterating over the domain of the function, and for each return function
 * iterating over that domain to get a sequence of the type of the inner function's target domain.
 * @param fun a curried function.
 * @tparam A1 argument of first function
 * @tparam A2 argument of inner functions.
 * @tparam B return type of inner functions.
 */
case class ImageSeq2[A1, A2, B](fun: Term[Fun[A1, Fun[A2, B]]]) extends Term[Seq[B]] {
  val FunTerm(funCandidateDom, _) = fun
  def eval(state: State) = for (f <- fun.eval(state); d <- funCandidateDom.eval(state)) yield {
    for (a1 <- d.view.toSeq; f1 = f(a1); a1 <- f1.funDom.view.toSeq) yield f1(a1)
  }
  def variables = fun.variables
  def domain[C >: Seq[B]] = Constant(new AllOfType[C])
  def default = fun.default.funRange.head.funRange.view.toSeq
}


trait UncurriedLambdaAbstraction[A1, R] {
  def lambda1: LambdaAbstraction[A1, _]
  def lambdaLast: LambdaAbstraction[_, R]
  def variables = lambda1.variables
  def funRange = lambdaLast.funRange
}

object Curried2 {
  def unapply[A1, A2, R](f: FunTerm[A1, Fun[A2, R]]) = {
    f match {
      case l1@LambdaAbstraction(v1, l2@LambdaAbstraction(v2, term)) => Some(UncurriedLambdaAbstraction2[A1, A2, R](l1, l2))
      case _ => None
    }
  }


}

case class UncurriedLambdaAbstraction2[A1, A2, R](lambda1: LambdaAbstraction[A1, Fun[A2, R]],
                                                  lambdaLast: LambdaAbstraction[A2, R])
  extends FunTerm[(A1, A2), R] with UncurriedLambdaAbstraction[A1, R] {

  def eval(state: State) = for (f <- lambda1.eval(state)) yield Fun({
    case (a1, a2) => f(a1)(a2)
  })
  def domain[C >: Fun[(A1, A2), R]] = Constant(Util.setToBeImplementedLater)
  def default = Fun({
    case (a1, a2) => lambda1.default(a1)(a2)
  })
  //todo: this may not work if the domain of the last variable depends on the first variable.
  def funCandidateDom = CartesianProductTerm2(lambda1.sig.domain, lambdaLast.sig.domain)
}

