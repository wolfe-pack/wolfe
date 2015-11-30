package ml.wolfe

import ml.wolfe.term._
import scala.language.implicitConversions

/**
 * @author riedel
 */
object Language extends NameProviderImplicits with SeqHelper with LinAlg {

  implicit def toLambdaAbstraction[A,B](f:Term[A] => Term[B]):LambdaAbstraction1[A,B] = {
    val arg = Var("_lambdaArg")
    val body = f(arg)
    LambdaAbstraction1(arg,body)
  }

  def Var[T](name: String) = new Var[T](name)

  def Var[T](implicit provider: NameProvider) = new Var[T](provider.newName())

  implicit def toConstant[T](value: T): Constant[T] = Constant(value)

  def sum[T, N](args: Term[Seq[T]])(obj: Term[T] => Term[N])(implicit numeric: Numeric[N]) =
    Sum(SeqMap(args, obj))

  implicit class BindingCreator[T](variable: Var[T]) {
    def :=(value: T) = Binding(variable, value)
  }

  implicit class DomBindingCreator[T](term: Term[T]) {
    def in(dom: Dom[T]) = DomainBinding(term, dom)
  }

  implicit class IntTerm(i: Term[Int]) {
    def until(to: Term[Int]) = RangeTerm(i, to)
  }

  implicit class VarCreator[T](dom: Dom[T]) {
    def Variable(name: String)(implicit domains: Domains): Var[T] = {
      val result = new Var[T](name)
      domains(result) = dom
      result
    }

    def Var(implicit provider: NameProvider, domains: Domains) = Variable(provider.newName())
  }

  implicit class NumericTerm[N](n: Term[N])(implicit val numeric: Numeric[N]) {
    def +(that: Term[N]) = Plus(n, that)

    def -(that: Term[N]) = Minus(n, that)

    def *(that: Term[N]) = Times(n, that)

    def unary_- = Times(Constant(numeric.fromInt(-1)), n)

  }

  implicit class Tuple2Term[T1, T2](t: Term[(T1, T2)]) {
    def _1 = GetElement[T1](t, 0)

    def _2 = GetElement[T1](t, 1)
  }

}

trait SeqHelper {

  def max(s1: Term[Seq[Double]], s2: Term[Seq[Double]]) = SeqPointWiseMax(s1, s2)

  def fill[E](length: Term[Int])(elem: Term[E]) = SeqFill(length, elem)

  implicit class SeqTerm[E](val s: Term[Seq[E]]) {
    def apply(i: Term[Int]) = SeqApply[E](s, i)

    def map[B](f: Term[E] => Term[B]) = SeqMap(s, Language.toLambdaAbstraction(f))

    def :+(that: Term[E]) = SeqAppend(s, that)

    def length = SeqLength(s)

    def slice(from: Term[Int], to: Term[Int]) = SeqSlice(s, from, to)

    //terminology from http://colah.github.io/posts/2015-09-NN-Types-FP/
    def foldl[S](init: Term[S])(op: (Term[S], Term[E]) => Term[S]): Term[S] = Foldl(s, init, op)

    //def unfoldr //needs to be applied to a term instead of a seq of terms

    def mapAccumR[S](init: Term[S])(op: (Term[S], Term[E]) => Term[S]): Term[Seq[S]] = MapAccumR(s, init, op)
  }

  implicit class NumericSeqTerm[E](val s: Term[Seq[E]])(implicit numeric: Numeric[E]) {
    def sum = Sum(s)
  }

  implicit class DoubleSeqTerm(val s: Term[Seq[Double]]) {
    def -(that: Term[Seq[Double]]) = SeqMinus(s, that)
  }


}

trait LinAlg {
  implicit class TensorHelper(term: Term[Tensor]) {
    def +(x: Term[Tensor]) = ComponentPlus(term, x)
    def :*(x: Term[Tensor]) = ComponentMul(term, x)
    def *(x: Term[Tensor]) = TensorMul(term, x)
    def t = Transpose(term)
  }

  def tanh(x: Term[Tensor]) = Tanh(x)
  def sigmoid(x: Term[Tensor]) = Sigmoid(x)
  def log(x: Term[Tensor]) = Log(x)
  def concat(args:Term[Tensor]*) = Concat(args.toSeq)
  def max(args:Term[Tensor]*) = Max(args.toSeq)
}
