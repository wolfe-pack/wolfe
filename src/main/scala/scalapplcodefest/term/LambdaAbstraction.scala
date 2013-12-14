package scalapplcodefest.term

import scalapplcodefest.value.{AllFunctions, AllOfType, Fun}
import scalapplcodefest.SetUtil

/**
 * A Lambda Abstraction corresponds to a function and consists of a variable and a term.
 * The functions value at a particular argument can be determined by evaluating the inner
 * term with the variable bound to the given argument.
 *
 * @param sig the argument signature of the function. A variable, or a tuple of variables.
 * @param body the term that when evaluated becomes the return value of the function.
 * @tparam A type of arguments to function.
 * @tparam B type of return values of function.
 */
case class LambdaAbstraction[A,B](sig:Sig[A],body:Term[B]) extends FunTerm[A,B] with Composite2[A,B,Fun[A,B]] {
  lambda =>

  def funCandidateDom = sig.dom
  def funRange = body.domain[B]
  def eval(state: State) = {
    for (r <- funRange.eval(state); d <- funCandidateDom.eval(state)) yield new Fun[A, B] {
      def funCandidateDom = d
      def funRange = r
      def get(a: A) = body.eval(state + sig.toState(a))
      def apply(a: A) = get(a).get
      override def isDefinedAt(a: A) = d(a) && get(a).exists(r(_))
    }
  }
  def variables = SetUtil.SetMinus(body.variables, sig.variables)
  def default = new Fun[A, B] {
    def funCandidateDom = lambda.funCandidateDom.default
    def funRange = lambda.funRange.default
    def apply(v1: A) = body.default
    def isDefinedAt(x: A) = sig.dom.default(x)
  }
  def domain[C >: Fun[A, B]] = Constant(new AllOfType[C])
  override def toString = s"lam $sig { $body }"
  def components = (sig,body)
  def copy(t1: Term[A], t2: Term[B]) = LambdaAbstraction(t1.asInstanceOf[Sig[A]],t2)
}




