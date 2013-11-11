package scalapplcodefest

/**
 * FunctionTerm evaluate to partial functions. The domain where the function is defined for
 * may depend on the state, and is hence a term itself.
 * The general contract for partial functions with the given function domain and range are as follows:
 * the function is undefined for all values outside the domain, and every result is part of the range. Note that
 * the function may also be undefined for some values inside the domain.
 * @tparam A type of arguments to function.
 * @tparam B type of return values of function.
 */
trait FunctionTerm[A, +B] extends Term[PartialFunction[A, B]] {
  def superDomain[C <: A]: Term[Set[C]]
  def superRange[D >: B]: Term[Set[D]]
}

/**
 * A function constant with restricted super domain and range.
 * @param function the partial function underlying the function.
 * @param dom a super domain (everything outside of this domain is undefined)
 * @param ran a super range (everything outside of this range can never be returned)
 * @tparam A type of arguments to function.
 * @tparam B type of return values of function.
 */
case class ConstantFunction[A, B](function: PartialFunction[A, B], dom: Term[Set[A]], ran: Term[Set[B]]) extends FunctionTerm[A, B] {

  import SetCastHelper._

  val restrictedFunction = new PartialFunction[A, B] {
    def apply(v1: A) = function(v1)
    def isDefinedAt(x: A) = {
      val d = dom.eval(State.empty)
      val r = ran.eval(State.empty)
      d.isDefined && function.isDefinedAt(x) && d.get(x) && r.isDefined && r.get(function(x))
    }
  }
  def superDomain[C <: A] = dom.as[C]
  def superRange[D >: B] = ran.as[D]
  def eval(state: State) = Some(restrictedFunction)
  def variables = dom.variables ++ ran.variables
  def domain[C >: PartialFunction[A, B]] = Constant(Set(restrictedFunction))
  def default = function
}

/**
 * Application of a function to an argument
 * @param function the function to apply
 * @param arg the argument to apply the function to
 * @tparam A argument type of function
 * @tparam B return type of function
 */
case class FunApp[A, B](function: FunctionTerm[A, B], arg: Term[A]) extends Term[B] {
  def eval(state: State) =
    for (f <- function.eval(state);
         a <- arg.eval(state);
         v <- f.lift(a)) yield v
  def variables = function match {
    case Predicate(_, dom, ran) => ???
    case _ => SetUtil.Union(Set(function.variables, arg.variables))
  }
  def default = function.default(function.superDomain.default.head)
  def domain[C >: B] = function.superRange[C]
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
case class LambdaAbstraction[A, B](variable: Variable[A], term: Term[B]) extends FunctionTerm[A, B] with Term[LambdaFunction[A,B]] {
  def superDomain[C <: A] = variable.domain.asInstanceOf[Term[Set[C]]]
  def superRange[D >: B]: Term[Set[D]] = term.domain[D]
  def eval(state: State) = {
    for (r <- superRange[B].eval(state); d <- superDomain[A].eval(state)) yield new LambdaFunction[A, B] {
      def get(a: A) = term.eval(state + SingletonState(variable, a))
      def apply(a: A) = get(a).get
      def isDefinedAt(a: A) = d(a) && get(a).isDefined
    }
  }
  def variables = SetUtil.SetMinus(term.variables,Set(variable))
  def default = new LambdaFunction[A,B] {
    def apply(v1: A) = term.default
    def isDefinedAt(x: A) = variable.domain.default(x)
  }
  def domain[C >: LambdaFunction[A,B]] = Constant(Util.setToBeImplementedLater)
}

trait LambdaFunction[A,B] extends PartialFunction[A,B]

trait UncurriedLambdaAbstraction[A1,R] {
  def lambda1:LambdaAbstraction[A1,_]
  def lambdaLast:LambdaAbstraction[_,R]
  def variables = lambda1.variables
  def superRange[D >: R] = lambdaLast.superRange[D]
}

object Curried2 {
  def unapply[A1,A2,R](f:FunctionTerm[A1,LambdaFunction[A2,R]]) = {
    f match {
      case l1@LambdaAbstraction(v1,l2@LambdaAbstraction(v2,term)) => Some(UncurriedLambdaAbstraction2[A1,A2,R](l1,l2))
      case _ => None
    }
  }

  def unroll[A1,A2,A3](term:TupleTerm2[(A1,A2),A3]):Option[TupleTerm3[A1,A2,A3]] = term match {
    case TupleTerm2(TupleTerm2(a1,a2),a3) => Some(TupleTerm3(a1,a2,a3))
    case _ => None
  }

}

case class UncurriedLambdaAbstraction2[A1, A2, R](lambda1: LambdaAbstraction[A1, LambdaFunction[A2, R]],
                                                  lambdaLast: LambdaAbstraction[A2, R])
  extends FunctionTerm[(A1, A2), R] with UncurriedLambdaAbstraction[A1,R] {

  import SetCastHelper._

  def eval(state: State) = for (f <- lambda1.eval(state)) yield {case (a1,a2) => f(a1)(a2)}
  def domain[C >: PartialFunction[(A1, A2), R]] = Constant(Util.setToBeImplementedLater)
  def default = {case (a1,a2) => lambda1.default(a1)(a2) }
  def superDomain[C <: (A1, A2)] = CartesianProductTerm2(lambda1.variable.domain,lambdaLast.variable.domain).as[C]
}
