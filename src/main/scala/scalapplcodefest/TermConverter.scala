package scalapplcodefest

import scala.language.implicitConversions

/**
 * @author Sebastian Riedel
 */
object TermConverter {

  import TermImplicits._

  def distributeConditions[T](term: Term[T], condition: State = State.empty): Term[T] = {
    val vars = term.variables
    term match {
      case t if !condition.domain.exists(vars(_)) => t
      case FunApp(f, a) => FunApp(FunTerm(distributeConditions(f, condition)), distributeConditions(a, condition))
      case SeqTerm(args) => SeqTerm(args.map(distributeConditions(_, condition))).asInstanceOf[Term[T]]
      case TupleTerm2(arg1, arg2) => TupleTerm2(distributeConditions(arg1, condition), distributeConditions(arg2, condition)).asInstanceOf[Term[T]]
      case _ => term | condition
    }
  }

  def flattenDoubleSums(term: Term[Double]): Seq[Term[Double]] = term match {
    case Math.DoubleAdd.Reduced(SeqTerm(args)) => args.flatMap(flattenDoubleSums)
    case Math.DoubleAdd.Applied(arg1, arg2) => flattenDoubleSums(arg1) ++ flattenDoubleSums(arg1)
    case _ => Seq(term)
  }

  def flattenDouble[T](term: Term[T]): Term[T] = {
    implicit def cast(t: Term[Any]) = t.asInstanceOf[Term[T]]
    term match {
      case FunTermProxy(self) => FunTermProxy(flattenDouble(self))
      case Math.DoubleAdd.Reduced(SeqTerm(args)) => dsum(SeqTerm(args.flatMap(flattenDoubleSums)))
      case Math.DoubleAdd.Applied(arg1, arg2) => dsum(SeqTerm(flattenDoubleSums(arg1) ++ flattenDoubleSums(arg1)))
      case f@FunApp(fun,arg) => FunApp(FunTerm(flattenDouble(fun)),flattenDouble(arg))
      case _ => term
    }
  }


  def unrollLambdaAbstractions2[T](term: Term[Seq[T]]): Term[Seq[T]] = term match {
    case ImageSeq(fun@LambdaAbstraction(v, arg)) => SeqTerm(State.allStates(List(v)).map(state => fun(arg | state)))
    case _ => term
  }

  def unrollLambdas[T](term: Term[T]): Term[T] = {
    implicit def cast(t: Term[Any]) = t.asInstanceOf[Term[T]]
    term match {
      case ImageSeq(fun@LambdaAbstraction(v, arg)) => SeqTerm(State.allStates(List(v)).map(state => fun(arg) | state))
      //      case Math.VecAdd.Reduced(args) => vsum(unrollLambdas(args))
      case r@Reduce(op, args) => r.copy(op = unrollLambdas(op).asInstanceOf[FunTerm[(T,T),T]], arguments = unrollLambdas(args))
      //      case r@Reduce(op,args) => Reduce(FunTerm(unrollLambdas(op)),unrollLambdas(args))//, arguments = unrollLambdas(args))
      case app@FunApp(f, a) => app.copy(function = FunTerm(unrollLambdas(f)), arg = unrollLambdas(a))
      case s@SeqTerm(args) => s.copy(seq = args.map(unrollLambdas)) //red in IDEA
      case t@TupleTerm2(arg1, arg2) => t.copy(a1 = unrollLambdas(arg1), a2 = unrollLambdas(arg2)) //red in IDEA
      case _ => term
    }
  }


  def flattenVectorSums(term: Term[Vector]): Seq[Term[Vector]] = term match {
    case Math.VecAdd.Reduced(SeqTerm(args)) => args.flatMap(flattenVectorSums)
    case Math.VecAdd.Applied(arg1, arg2) => flattenVectorSums(arg1) ++ flattenVectorSums(arg1)
    case _ => Seq(term)
  }

  def distDots(term: Term[Double]): Term[Double] = {
    term match {
      case LinearModel(f, w, base) => distDots(f dot w) + base
      case Math.Dot.Applied(Math.VecAdd.Applied(f1, f2), w) => distDots(f1 dot w) + distDots(f2 dot w)
      case Math.Dot.Applied(Math.VecAdd.Reduced(SeqTerm(args)), w) => dsum(SeqTerm(args.map(a => distDots(a dot w))))
      case Math.Dot.Applied(Quantified.VecSum(ImageSeq(LambdaAbstraction(v, arg))), w) =>
        dsum(LambdaAbstraction(v, distDots(arg dot w)))
      case _ => term
    }
  }

}


