package scalapplcodefest

import scala.language.implicitConversions
import scalapplcodefest.Math.UnitVec

/**
 * This object provides a set of implicit conversions that allow users
 * to write down terms more compactly.
 *
 * @author Sebastian Riedel
 */
object TermImplicits {

  implicit def intToConstant(x: Int) = Constant(x)
  implicit def doubleToConstant(x: Double) = Constant(x)
  implicit def setToConstant[T](x: Set[T]) = Constant(x)
  implicit def toTupleTerm2[T1,T2](tuple:(Term[T1],Term[T2])) = TupleTerm2(tuple._1,tuple._2)
  implicit def toRichTerm[T](term:Term[T]) = RichTerm(term)
  implicit def toRichInt(i: Term[Int]) = RichIntTerm(i)
  implicit def toRichFunction[A, B](f: FunTerm[A, B]) = RichFunctionTerm(f)
  implicit def toRichFunction2[A1, A2, B](f: FunTerm[(A1, A2), B]) = RichFunctionTerm2(f)
  implicit def toRichPredicate[A, B](p: Predicate[A, B]) = RichPredicate(p)
  implicit def toRichPredicate2[A1, A2, B](p: Predicate[(A1, A2), B]) = RichPredicate2(p)
  implicit def toRichSetTerm[T](s: Term[Set[T]]) = RichSetTerm(s)
  implicit def toFinishedCartesianProduct[A](unfinished: UnfinishedCartesianProduct[A]) = unfinished.finish

  //math
  def e_(index:Term[Int],value:Term[Double] = Constant(1.0)) = UnitVec(index,value)



  case class RichSetTerm[T](s: Term[Set[T]]) {

    def freshVariable[A](dom: Term[Set[A]] = s) = new Variable[A] {
      def domain[C >: A] = dom.asInstanceOf[Term[Set[C]]]
    }

    def map[R](f: Variable[T] => Term[R]): LambdaAbstraction[T, R] = {
      val variable = freshVariable()
      LambdaAbstraction(variable, f(variable))
    }
    def flatMap[A1, A2](f: Variable[T] => LambdaAbstraction[A1, A2]) = {
      val variable: Variable[T] = freshVariable()
      val innerLambda = f(variable)
      LambdaAbstraction(variable,innerLambda)
    }

    def x[T2](that: Term[Set[T2]]) = UnfinishedCartesianProduct2(s, that)

  }

  trait UnfinishedCartesianProduct[T] {
    def finish: Term[Set[T]]
  }

  case class RichTerm[T](term:Term[T]) {
    def |(condition:State) = Conditioned(term,condition)
    def |(mappings:(Variable[Any],Any)*) = Conditioned(term,State(mappings.toMap))
  }

  case class UnfinishedCartesianProduct2[T1, T2](dom1: Term[Set[T1]], dom2: Term[Set[T2]])
    extends UnfinishedCartesianProduct[(T1, T2)] {
    def x[T3](that: Term[Set[T3]]) = UnfinishedCartesianProduct3(dom1, dom2, that)
    def finish = CartesianProductTerm2(dom1, dom2)
  }

  case class UnfinishedCartesianProduct3[T1, T2, T3](dom1: Term[Set[T1]], dom2: Term[Set[T2]], dom3: Term[Set[T3]])
    extends UnfinishedCartesianProduct[(T1, T2, T3)] {
    def finish = CartesianProductTerm3(dom1, dom2, dom3)
  }


  case class RichIntTerm(i: Term[Int]) {
    def +(that: Term[Int]) = FunApp(ConstantFun(Math.IntAdd), TupleTerm2(i, that))
  }

  case class RichFunctionTerm[A, B](f: FunTerm[A, B]) {
    def apply(a: Term[A]) = FunApp(f, a)
  }

  case class RichFunctionTerm2[A1, A2, B](f: FunTerm[(A1, A2), B]) {
    def apply(a1: Term[A1], a2: Term[A2]) = FunApp(f, TupleTerm2(a1, a2))
  }


  case class RichPredicate[A, B](p: Predicate[A, B]) {
    def atom(a: A) = GroundAtom(p, a)
  }

  case class RichPredicate2[A1, A2, B](p: Predicate[(A1, A2), B]) {
    def atom(a1: A1, a2: A2) = GroundAtom(p, (a1, a2))
  }

}
