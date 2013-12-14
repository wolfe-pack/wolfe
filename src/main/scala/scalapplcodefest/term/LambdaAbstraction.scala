package scalapplcodefest.term

import scalapplcodefest.value.{AllOfType, Fun}
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

/**
 * A signature for lambda abstractions. A basic signature is a tuple of variables, and each variable
 * can appear in the body of the lambda abstraction.
 *
 * @tparam T type of values that the signature describes.
 */
sealed trait Sig[T] extends Term[T] {
  def variables:Set[Variable[Any]]
  def toState(value:T):State
  def dom:Term[Set[T]]
  def domain[C >: T] = dom.asInstanceOf[Term[Set[C]]]
  //def forVariable(variable:Variable[Any],value:T): Any Or Undefined
}

case class VarSig[T](variable:Variable[T]) extends Sig[T] with Composite1[T,T]{
  def eval(state: State) = for (v1 <- variable.eval(state)) yield v1
  def default = variable.default
  def toState(value: T) = State(Map(variable -> value))
  def variables = Set(variable)
  def dom = variable.domain
  def components = variable
  def copy(t1: Term[T]) = VarSig(t1.asInstanceOf[Variable[T]])
  override def toString = variable.toString
}
case class TupleSig2[T1,T2](sig1:Sig[T1],sig2:Sig[T2]) extends Sig[(T1,T2)] with Composite2[T1,T2,(T1,T2)] {
  def eval(state: State) = for (v1 <- sig1.eval(state); v2 <- sig2.eval(state)) yield (v1,v2)
  def default = (sig1.default,sig2.default)
  def toState(value: (T1, T2)) = sig1.toState(value._1) + sig2.toState(value._2)
  def variables = sig1.variables ++ sig2.variables
  def dom = CartesianProductTerm2(sig1.dom,sig2.dom)
  def components = (sig1,sig2)
  def copy(t1: Term[T1], t2: Term[T2]) = TupleSig2(t1.asInstanceOf[Sig[T1]],t2.asInstanceOf[Sig[T2]])
  override def toString = s"($sig1,$sig2)"
}
case class TupleSig3[T1,T2,T3](sig1:Sig[T1],sig2:Sig[T2],sig3:Sig[T3])
  extends Sig[(T1,T2,T3)] with Composite3[T1,T2,T3,(T1,T2,T3)] {

  def eval(state: State) = for (v1 <- sig1.eval(state); v2 <- sig2.eval(state); v3 <- sig3.eval(state)) yield (v1,v2,v3)
  def default = (sig1.default,sig2.default,sig3.default)
  def toState(value: (T1, T2, T3)) = sig1.toState(value._1) + sig2.toState(value._2) + sig3.toState(value._3)
  def variables = sig1.variables ++ sig2.variables ++ sig3.variables
  def dom = CartesianProductTerm3(sig1.dom,sig2.dom,sig3.dom)
  def components = (sig1,sig2,sig3)
  def copy(t1: Term[T1], t2: Term[T2], t3:Term[T3]) = TupleSig3(t1.asInstanceOf[Sig[T1]],t2.asInstanceOf[Sig[T2]],t3.asInstanceOf[Sig[T3]])

  override def toString = s"($sig1,$sig2,$sig3)"
}

