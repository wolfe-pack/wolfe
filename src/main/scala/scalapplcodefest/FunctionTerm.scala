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
trait FunctionTerm[-A, +B] extends Term[PartialFunction[A, B]] {
  def superDomain[C <: A]: Term[Set[C]]
  def superRange[D >: B]: Term[Set[D]]
}

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
case class LambdaAbstraction[A, B](variable: Variable[A], term: Term[B]) extends FunctionTerm[A, B] {
  def superDomain[C <: A] = variable.domain.asInstanceOf[Term[Set[C]]]
  def superRange[D >: B]: Term[Set[D]] = term.domain[D]
  def eval(state: State) = {
    for (r <- superRange[B].eval(state); d <- superDomain[A].eval(state)) yield new PartialFunction[A, B] {
      def get(a: A) = term.eval(state + SingletonState(variable, a))
      def apply(a: A) = get(a).get
      def isDefinedAt(a: A) = d(a) && get(a).isDefined
    }
  }
  def variables = term.variables
  def default = {
    case a => term.default
  }
  def domain[C >: PartialFunction[A, B]] = Constant(Util.setToBeImplementedLater)
}

