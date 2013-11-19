package scalapplcodefest

import scala.language.implicitConversions

/**
 * @author Sebastian Riedel
 */
object TermConverter {

  import TermImplicits._

  def distConds[T](term: Term[T]): Term[T] = {
    implicit def cast(t: Term[Any]) = t.asInstanceOf[Term[T]]
    term match {
      case Conditioned(const@Constant(_),_) => const
      case Conditioned(LinearModel(feats,weights,base),c) => LinearModel(distConds(feats | c),weights, distConds(base|c))
      case Conditioned(Math.VecAdd.Reduced(args),c) => vsum(distConds(args | c))
      case Conditioned(Math.VecAdd.Applied2(arg1,arg2),c) => distConds(arg1 | c) + distConds(arg2 | c)
      case Conditioned(Math.DoubleAdd.Reduced(args),c) => dsum(distConds(args | c))
      case Conditioned(Math.DoubleAdd.Applied2(arg1,arg2),c) => distConds(arg1 | c) + distConds(arg2 | c)
      case Conditioned(SeqTerm(args),c) => SeqTerm(args.map(a => distConds(a | c)))
      case Conditioned(ImageSeq(LambdaAbstraction(v,arg)),c) => ImageSeq(LambdaAbstraction(v,distConds(arg | c)))
      case _ => term
    }
  }

  def flattenDoubleSums(term: Term[Double]): Seq[Term[Double]] = {
    val raw = term match {
      case Math.DoubleAdd.Reduced(SeqTerm(args)) => args.flatMap(flattenDoubleSums)
      case Math.DoubleAdd.Applied2(arg1, arg2) => flattenDoubleSums(arg1) ++ flattenDoubleSums(arg1)
      case _ => Seq(term)
    }
    raw.filter(_ != Constant(0.0))
  }

  def flattenDouble[T](term: Term[T]): Term[T] = {
    implicit def cast(t: Term[Any]) = t.asInstanceOf[Term[T]]
    term match {
      case FunTermProxy(self) => FunTermProxy(flattenDouble(self))
      case Math.DoubleAdd.Reduced(SeqTerm(args)) => dsum(SeqTerm(args.flatMap(flattenDoubleSums)))
      case Math.DoubleAdd.Applied2(arg1, arg2) => dsum(SeqTerm(flattenDoubleSums(arg1) ++ flattenDoubleSums(arg2)))
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
      case ImageSeq(fun@LambdaAbstraction(v, arg)) => SeqTerm(State.allStates(List(v)).map(state => arg | state))
      case r@Reduce(op, args) => r.copy(op = unrollLambdas(op).asInstanceOf[FunTerm[(T,T),T]], arguments = unrollLambdas(args))
      case app@FunApp(f, a) => app.copy(function = FunTerm(unrollLambdas(f)), arg = unrollLambdas(a))
      case s@SeqTerm(args) => s.copy(seq = args.map(unrollLambdas)) //red in IDEA
      case t@TupleTerm2(arg1, arg2) => t.copy(a1 = unrollLambdas(arg1), a2 = unrollLambdas(arg2)) //red in IDEA
      case _ => term
    }
  }


  def flattenVectorSums(term: Term[Vector]): Seq[Term[Vector]] = term match {
    case Math.VecAdd.Reduced(SeqTerm(args)) => args.flatMap(flattenVectorSums)
    case Math.VecAdd.Applied2(arg1, arg2) => flattenVectorSums(arg1) ++ flattenVectorSums(arg1)
    case _ => Seq(term)
  }

  def distDots(term: Term[Double]): Term[Double] = {
    term match {
      case Conditioned(t,c) => Conditioned(distDots(t),c)
      case LinearModel(f, w, base) => distDots(f dot w) + base
      case Math.Dot.Applied2(Math.VecAdd.Applied2(f1, f2), w) => distDots(f1 dot w) + distDots(f2 dot w)
      case Math.Dot.Applied2(Math.VecAdd.Reduced(SeqTerm(args)), w) => dsum(SeqTerm(args.map(a => distDots(a dot w))))
      case Math.Dot.Applied2(Quantified.VecSum(ImageSeq(LambdaAbstraction(v, arg))), w) =>
        dsum(LambdaAbstraction(v, distDots(arg dot w)))
      case _ => term
    }
  }

  def simplifyConds[T](term:Term[T]):Term[T] = {
    term match {
      case Conditioned(Conditioned(t,c1),c2) => Conditioned(t,c1 + c2)
      case app@FunApp(f,a) => app.copy(FunTerm(simplifyConds(f)),simplifyConds(a))
      case _ => term
    }
  }

}


