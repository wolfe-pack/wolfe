package scalapplcodefest

import org.scalautils.{Bad, Good}

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
  import TermImplicits._
  def funCandidateDom: Term[Set[A]]
  def funRange: Term[Set[B]]
  def funDom = funCandidateDom.filteredBy(this.isDefined)
}

/**
 * A proxy for terms that evaluate to Fun objects, but are not implementing the FunTerm interface
 * @param self the wrapped term.
 */
case class FunTermProxy[A, B](self: Term[Fun[A, B]]) extends FunTerm[A, B] with ProxyTerm[Fun[A, B]] {
  def funCandidateDom = Constant(new AllOfType[A])
  def funRange = Constant(new AllOfType[B])
}

/**
 * Helper object to build FunTerms
 */
object FunTerm {
  /**
   * Returns term that corresponds to creating the set of all possible functions
   * given a domain and range.
   */
  def allFunctions[A, B] = ConstantFun(new AllFunctionsOp[A, B])

  /**
   * Turns a function term into a FunTerm
   */
  def apply[A, B](f: Term[Fun[A, B]]): FunTerm[A, B] = f match {
    case ft: FunTerm[_, _] => ft.asInstanceOf[FunTerm[A, B]]
    case _ => FunTermProxy(f)
  }


}

object MoreFun {
  def apply[A,B](f:PartialFunction[A,B]) = Constant(Fun(f, new AllOfType[A], new AllOfType[B]))

//  def apply[A,B](f:A=>B, p:A=>Boolean = (x:A) => true) = Constant( new PartialFunction[A,B] {
//    def apply(v1: A) = f(v1)
//    def isDefinedAt(x: A) = p(x)
//  }, new AllOfType[A], new AllOfType[B])
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
case class DynFunTerm[A,B](fun:PartialFunction[A,B], funCandidateDom:Term[Set[A]], funRange:Term[Set[B]]) extends FunTerm[A,B] {
  def variables = funCandidateDom.variables ++ funRange.variables
  def default = Fun(fun,funCandidateDom.default,funRange.default)
  def domain[C >: Fun[A, B]] = Constant(new AllOfType[C])
  def eval(state: State) = for (d <- funCandidateDom.eval(state); r <- funRange.eval(state)) yield Fun(fun,d,r)
}

case class Dyn[A,B](fun:AnyFunction,
                    funCandidateDom:Term[Set[A]] = Constant(new AllOfType[A]),
                    funRange:Term[Set[B]] = Constant(new AllOfType[B])) extends FunTerm[A,B] {
  def cast = fun.asInstanceOf[PartialFunction[A,B]]
  def variables = funCandidateDom.variables ++ funRange.variables
  def default = Fun(cast,funCandidateDom.default,funRange.default)
  def domain[C >: Fun[A, B]] = Constant(new AllOfType[C])
  def eval(state: State) = for (d <- funCandidateDom.eval(state); r <- funRange.eval(state)) yield Fun(cast,d,r)
}

/**
 * Fixed function with fixed domain and range.
 * @param fun the function this term is always evaluated to.
 * @tparam A type of arguments to function.
 * @tparam B type of return values of function.
 */
case class ConstantFun[A, B](fun: Fun[A, B]) extends FunTerm[A, B] {
  def eval(state: State) = Good(fun)
  def variables = Set.empty
  def domain[C >: Fun[A, B]] = Constant(Set(fun))
  def default = fun
  def funCandidateDom = Constant(fun.funCandidateDom)
  def funRange = Constant(fun.funRange)
  override def toString = fun.toString()
}

/**
 * Application of a function to an argument
 * @param function the function to apply
 * @param arg the argument to apply the function to
 * @tparam A argument type of function
 * @tparam B return type of function
 */
case class FunApp[A, B](function: FunTerm[A, B], arg: Term[A]) extends Term[B] {
  def eval(state: State) =
    for (f <- function.eval(state);
         a <- arg.eval(state);
         v <- f.lift(a).map(Good(_)).getOrElse(Bad(FunctionNotDefinedAt(this,state)))) yield v
  def variables = function match {
    case p@Predicate(n, d, r) => PartialGroundAtoms(p, arg)
    case _ => SetUtil.SetUnion(List(function.variables, arg.variables))
  }
  def default = function.default(function.funCandidateDom.default.head)
  def domain[C >: B] = Image(function, arg.domain).asInstanceOf[Term[Set[C]]] //replace by function.funDomain collectedBy function?
  //could also be function.funRange
  override def toString = s"$function($arg)"
}

case object BinaryFunApp {
  def unapply[T](term: FunApp[(T, T), T]) = term match {
    case FunApp(f, a) => Some((f, a))
    case _ => None
  }
}

/**
 * A Lambda Abstraction corresponds to a function and consists of a variable and a term.
 * The functions value at a particular argument can be determined by evaluating the inner
 * term with the variable bound to the given argument.
 *
 * @param variable the variable that corresponds to the argument of the function.
 * @param term the term that when evaluated becomes the return value of the function.
 * @tparam A type of arguments to function.
 * @tparam B type of return values of function.
 */
case class LambdaAbstraction[A, B](variable: Variable[A], term: Term[B]) extends FunTerm[A, B] {
  //todo: variable should be a Var?

  lambda =>

  def funCandidateDom = variable.domain
  def funRange = term.domain[B]
  def eval(state: State) = {
    for (r <- funRange.eval(state); d <- funCandidateDom.eval(state)) yield new Fun[A, B] {
      def funCandidateDom = d
      def funRange = r
      def get(a: A) = term.eval(state + SingletonState(variable, a))
      def apply(a: A) = get(a).get
      override def isDefinedAt(a: A) = d(a) && get(a).exists(r(_))
    }
  }
  def variables = SetUtil.SetMinus(term.variables, Set(variable))
  def default = new Fun[A, B] {
    def funCandidateDom = lambda.funCandidateDom.default
    def funRange = lambda.funRange.default
    def apply(v1: A) = term.default
    def isDefinedAt(x: A) = variable.domain.default(x)
  }
  def domain[C >: Fun[A, B]] = Constant(new AllOfType[C])
    //FunApp(FunTerm.allFunctions[A, B], TupleTerm2(funCandidateDom, funRange)).asInstanceOf[Term[Set[C]]]
  override def toString = s"lam $variable :${variable.domain} { $term }"
}

/**
 * Set of a functions from domain to range
 * @param domain the domain of the functions
 * @param range the range of the functions
 * @tparam A argument type.
 * @tparam B return type.
 */
case class AllFunctions[A, B](domain: Set[A], range: Set[B]) extends SetValue[Fun[A, B]] {

  self =>

  def contains(elem: Fun[A, B]) = elem.funDom == domain && elem.funRange == range
  def iterator = {
    def allFunctions(d: List[A], r: List[B], funs: List[Fun[A, B]] = List(Fun.empty)): List[Fun[A, B]] = {
      d match {
        case Nil => funs
        case newArg :: tail =>
          val newFunctions = for (v <- r; f <- funs) yield new Fun[A, B] {
            def funCandidateDom = self.domain
            def funRange = range
            override def funDom = self.domain
            def apply(v1: A) = if (v1 == newArg) v else f(v1)
            def isDefinedAt(x: A) = x == newArg || f.isDefinedAt(x)
          }
          allFunctions(tail, r, newFunctions)
      }
    }
    allFunctions(domain.toList, range.toList).iterator
  }
}

class AllFunctionsOp[A, B] extends Fun[(Set[A], Set[B]), Set[Fun[A, B]]] {
  def funCandidateDom = new AllOfType[(Set[A], Set[B])]
  override def funDom = funCandidateDom
  def funRange = new AllOfType[Set[Fun[A, B]]]
  def isDefinedAt(x: (Set[A], Set[B])) = true
  def apply(v1: (Set[A], Set[B])) = AllFunctions(v1._1, v1._2)
}


/**
 * The Image of a function term is the set of return values we get by applying
 * the function to all elements of its domain.
 * @param fun the function to get the image for
 * @tparam A argument type of function.
 * @tparam B return type of function.
 */
case class Image[A, B](fun: FunTerm[A, B], dom: Term[Set[A]]) extends Term[Set[B]] {
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
case class ImageSeq1[A, B](fun: FunTerm[A, B]) extends Term[Seq[B]] {
  def eval(state: State) = for (f <- fun.eval(state); d <- fun.funCandidateDom.eval(state)) yield d.view.toSeq.map(f)
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
case class ImageSeq2[A1, A2, B](fun: FunTerm[A1, Fun[A2, B]]) extends Term[Seq[B]] {
  def eval(state: State) = for (f <- fun.eval(state); d <- fun.funCandidateDom.eval(state)) yield {
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
  def funCandidateDom = CartesianProductTerm2(lambda1.variable.domain, lambdaLast.variable.domain)
}
