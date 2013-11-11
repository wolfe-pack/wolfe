package scalapplcodefest

/**
 * FunTerms evaluate to partial functions. The domain where the function is defined for
 * may depend on the state, and is hence a term itself.
 * The general contract for partial functions with the given function domain and target set are as follows:
 * the function is undefined for all values outside the domain, and every result is part of the target set. Note that
 * the function may also be undefined for some values inside the (super) domain.
 * @tparam A type of arguments to function.
 * @tparam B type of return values of function.
 */
trait FunTerm[A, B] extends Term[Fun[A, B]] {
  def superDomain: Term[Set[A]]
  def targetSet: Term[Set[B]]
}

/**
 * Partial functions with invariant argument and result types. This is important for pattern matching.
 * @tparam A argument type.
 * @tparam B return type.
 */
trait Fun[A,B] extends PartialFunction[A,B]

/**
 * Helper object to build Fun objects.
 */
object Fun {
  def apply[A,B](f:PartialFunction[A,B]) = new Fun[A,B] {
    def apply(v1: A) = f.apply(v1)
    def isDefinedAt(x: A) = f.isDefinedAt(x)
  }
}

/**
 * A function constant with restricted super domain and range.
 * @param function the partial function underlying the function.
 * @param superDomain a super domain (everything outside of this domain is undefined)
 * @param targetSet a super range (everything outside of this range can never be returned)
 * @tparam A type of arguments to function.
 * @tparam B type of return values of function.
 */
case class ConstantFun[A, B](function: Fun[A, B], superDomain: Term[Set[A]], targetSet: Term[Set[B]]) extends FunTerm[A, B] {

  val restrictedFunction = new Fun[A, B] {
    def apply(v1: A) = function(v1)
    def isDefinedAt(x: A) = {
      val d = superDomain.eval(State.empty)
      val r = targetSet.eval(State.empty)
      d.isDefined && function.isDefinedAt(x) && d.get(x) && r.isDefined && r.get(function(x))
    }
  }
  def eval(state: State) = Some(restrictedFunction)
  def variables = superDomain.variables ++ targetSet.variables
  def domain[C >: Fun[A, B]] = Constant(Set(restrictedFunction))
  def default = function
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
         v <- f.lift(a)) yield v
  def variables = function match {
    //case Predicate(_, dom, ran) => ???
    case _ => SetUtil.SetUnion(List(function.variables, arg.variables))
  }
  def default = function.default(function.superDomain.default.head)
  def domain[C >: B] = function.targetSet.asInstanceOf[Term[Set[C]]]
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
case class LambdaAbstraction[A, B](variable: Variable[A], term: Term[B]) extends FunTerm[A, B]  {
  def superDomain = variable.domain
  def targetSet = term.domain[B]
  def eval(state: State) = {
    for (r <- targetSet.eval(state); d <- superDomain.eval(state)) yield new Fun[A, B] {
      def get(a: A) = term.eval(state + SingletonState(variable, a))
      def apply(a: A) = get(a).get
      def isDefinedAt(a: A) = d(a) && get(a).isDefined
    }
  }
  def variables = SetUtil.SetMinus(term.variables,Set(variable))
  def default = new Fun[A,B] {
    def apply(v1: A) = term.default
    def isDefinedAt(x: A) = variable.domain.default(x)
  }
  def domain[C >: Fun[A,B]] = Constant(Util.setToBeImplementedLater)
}

/**
 * The Image of a function term is the set of return values we get by applying
 * the function to all elements of its domain.
 * @param fun the function to get the image for
 * @tparam A argument type of function.
 * @tparam B return type of function.
 */
case class Image[A,B](fun:FunTerm[A,B]) extends Term[Set[B]] {
  def eval(state: State) = ???
  def variables = ???
  def domain[C >: Set[B]] = ???
  def default = ???
}


trait UncurriedLambdaAbstraction[A1,R] {
  def lambda1:LambdaAbstraction[A1,_]
  def lambdaLast:LambdaAbstraction[_,R]
  def variables = lambda1.variables
  def targetSet = lambdaLast.targetSet
}

object Curried2 {
  def unapply[A1,A2,R](f:FunTerm[A1,Fun[A2,R]]) = {
    f match {
      case l1@LambdaAbstraction(v1,l2@LambdaAbstraction(v2,term)) => Some(UncurriedLambdaAbstraction2[A1,A2,R](l1,l2))
      case _ => None
    }
  }

}

case class UncurriedLambdaAbstraction2[A1, A2, R](lambda1: LambdaAbstraction[A1, Fun[A2, R]],
                                                  lambdaLast: LambdaAbstraction[A2, R])
  extends FunTerm[(A1, A2), R] with UncurriedLambdaAbstraction[A1,R] {

  def eval(state: State) = for (f <- lambda1.eval(state)) yield Fun({case (a1,a2) => f(a1)(a2)})
  def domain[C >: Fun[(A1, A2), R]] = Constant(Util.setToBeImplementedLater)
  def default = Fun({case (a1,a2) => lambda1.default(a1)(a2)})
  def superDomain = CartesianProductTerm2(lambda1.variable.domain,lambdaLast.variable.domain)
}
