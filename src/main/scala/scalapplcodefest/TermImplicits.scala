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
  implicit def intToTerm(x: Int) = RichIntTerm(x)
  implicit def doubleToConstant(x: Double) = Constant(x)
  implicit def symbolToConstant(x: Symbol) = Constant(x)

  implicit def setToConstant[T](x: Set[T]) = Constant(x)
  //implicit def funToConstant[A,B](x:Fun[A,B]) = x)
  implicit def toTupleTerm2[T1, T2](tuple: (Term[T1], Term[T2])) = TupleTerm2(tuple._1, tuple._2)
  implicit def toRichVariable[T](v: Variable[T]) = RichVariable(v)
  implicit def toRichTerm[T](term: Term[T]) = new RichTerm(term)
  implicit def toRichInt[A](i: Term[Int]) = RichIntTerm(i)
  implicit def toRichDouble[A](t: Term[Double]) = RichDoubleTerm(t)
  implicit def toRichBooleanTerm[A](t: Term[Boolean]) = RichBooleanTerm(t)

  implicit def toRichFunTerm[A, B](term: Term[Fun[A, B]]): RichFunTerm[A, B] = term match {
    case f: FunTerm[_, _] => RichFunTerm(f).asInstanceOf[RichFunTerm[A, B]]
    case f => RichFunTerm(FunTerm(f))
  }
  implicit def toRichFunctionSeq[A, B](f: FunTerm[Seq[A], B]) = RichFunctionTermSeq(f)
  implicit def toRichFunction2[A1, A2, B](f: FunTerm[(A1, A2), B]) = RichFunctionTerm2(f)
  implicit def toRichPredicate[A, B](p: Predicate[A, B]) = RichPredicate(p)
  implicit def toRichPredicate2[A1, A2, B](p: Predicate[(A1, A2), B]) = RichPredicate2(p)
  implicit def toRichSetTerm[T](s: Term[Set[T]]) = RichSetTerm(s)
  implicit def toRichVec(term: Term[Vector]) = RichVecTerm(term)
  implicit def toRichVarSymbol(symbol: Symbol) = RichVarSymbol(symbol)
  implicit def toRichPredSymbol(symbol: Symbol) = RichPredSymbol(symbol)

  implicit def toFinishedCartesianProduct[A](unfinished: UnfinishedCartesianProduct[A]) = unfinished.finish

  //math
  def e_(index: Term[Int], value: Term[Double] = Constant(1.0)) = UnitVec(index, value)
  def I(term: Term[Boolean]) = FunApp(Math.Iverson.Term, term)

  def dsum(args: Term[Seq[Double]]) = Quantified.DoubleSum(args)
  def vsum(args: Term[Seq[Vector]]) = Quantified.VecSum(args)

  implicit def toImageSeq[A, B](f: FunTerm[A, B]) = ImageSeq(f)
  implicit def toImageSeqCurried2[A1, A2, B](f: FunTerm[A1, Fun[A2, B]]) = ImageSeqCurried2(f)


  implicit def uncurry[A1, A2, R](f: FunTerm[A1, Fun[A2, R]]) = f match {
    case Curried2(uncurried) => uncurried
    case _ => ???
  }

  case class RichVarSymbol(symbol: Symbol) {
    def of[T](set: Term[Set[T]]) = Var(symbol, set)
    def of[T](set: Set[T]) = Var(symbol, Constant(set))

  }

  case class RichPredSymbol(symbol: Symbol) {
    def of[A, B](domRange: (Term[Set[A]], Term[Set[B]])) = Predicate(symbol, domRange._1, domRange._2)
  }

  case class RichVariable[T](v: Variable[T]) {
    //def ->(value:T) = VarValuePair(v,value)
  }

  case class RichVecTerm(term: Term[Vector]) {
    def dot(that: Term[Vector]) = FunApp(Math.Dot.Term, TupleTerm2(term, that))
    def +(that: Term[Vector]) = FunApp(Math.VecAdd.Term, TupleTerm2(term, that))
  }

  private var anonVarCount = 0
  def freshName() = {
    this.synchronized {
      anonVarCount += 1
    }
    "_x" + anonVarCount
  }

  case class RichSetTerm[T](s: Term[Set[T]]) {

    def freshVariable[A](dom: Term[Set[A]] = s) = Var(Symbol(freshName()),dom)

    def map[R](f: Variable[T] => Term[R]): LambdaAbstraction[T, R] = {
      val variable = freshVariable()
      LambdaAbstraction(variable, f(variable))
    }
    def flatMap[A1, A2](f: Variable[T] => LambdaAbstraction[A1, A2]) = {
      val variable: Variable[T] = freshVariable()
      val innerLambda = f(variable)
      LambdaAbstraction(variable, innerLambda)
    }

    def x[T2](that: Term[Set[T2]]) = UnfinishedCartesianProduct2(s, that)

    def |->[T2](that: Term[Set[T2]]) = (s, that)

  }

  trait UnfinishedCartesianProduct[T] {
    def finish: Term[Set[T]]
  }

  case class VarValuePair[T](variable: Variable[T], value: T)

  class RichTerm[T](term: Term[T]) {
    def |(condition: State) = Conditioned(term, condition)
    def |(mappings: (Variable[Any], Any)*) = Conditioned(term, State(mappings.toMap))
    def eval(state: (Variable[Any], Any)*) = term.eval(State(state.toMap))
    //    def eval(state:VarValuePair[T]*):Option[T] = term.eval(State(state.map(_.toTuple).toMap))

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
    def +(that: Term[Int]) = FunApp(Math.IntAdd.Term, TupleTerm2(i, that))
    def -(that: Term[Int]) = FunApp(Math.IntMinus.Term, TupleTerm2(i, that))
    def ~~(that: Term[Int]) = RangeSet(i, that)
  }

  case class RichDoubleTerm(x: Term[Double]) {
    def +(that: Term[Double]) = FunApp(Math.DoubleAdd.Term, TupleTerm2(x, that))
    def *(that: Term[Double]) = FunApp(Math.DoubleMultiply.Term, TupleTerm2(x, that))
  }

  case class RichBooleanTerm(x: Term[Boolean]) {
    def &&(that: Term[Boolean]) = FunApp(Logic.And.Term, TupleTerm2(x, that))
    def ||(that: Term[Boolean]) = FunApp(Logic.Or.Term, TupleTerm2(x, that))
    def |=>(that: Term[Boolean]) = FunApp(Logic.Implies.Term, TupleTerm2(x, that))
    def unary_! = FunApp(Logic.Neg.Term, x)
    def unary_$ = FunApp(Math.Iverson.Term, x)
  }


  case class RichFunTerm[A, B](f: FunTerm[A, B]) {
    def apply(a: Term[A]) = FunApp(f, a)
  }

  case class RichFunctionTerm2[A1, A2, B](f: FunTerm[(A1, A2), B]) {
    def apply(a1: Term[A1], a2: Term[A2]) = FunApp(f, TupleTerm2(a1, a2))
  }

  case class RichFunctionTermSeq[A, B](f: FunTerm[Seq[A], B]) {
    def apply[C](args: Term[A]*)(implicit convert: C => Term[A]) = FunApp(f, SeqTerm(args.toSeq))
  }


  case class RichPredicate[A, B](p: Predicate[A, B]) {
    def atom(a: A) = GroundAtom(p, a)
  }

  case class RichPredicate2[A1, A2, B](p: Predicate[(A1, A2), B]) {
    def atom(a1: A1, a2: A2) = GroundAtom(p, (a1, a2))
  }

}
