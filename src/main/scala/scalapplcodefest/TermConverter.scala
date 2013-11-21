package scalapplcodefest

import scala.language.implicitConversions

/**
 * @author Sebastian Riedel
 */
object TermConverter {

  import TermImplicits._

  trait Converter {
    def convert[T](term: Term[T]): Term[T]
  }

  def convertDepthFirst[T](term: Term[T], converter: Converter): Term[T] = {
    implicit def cast(t: Term[Any]) = t.asInstanceOf[Term[T]]
    term match {
      case FunTermProxy(t) => converter.convert(FunTermProxy(convertDepthFirst(t, converter)))
      case TupleTerm2(a1, a2) => converter.convert(TupleTerm2(convertDepthFirst(a1, converter), convertDepthFirst(a2, converter)))
      case FunApp(f, a) => converter.convert(FunApp(FunTerm(convertDepthFirst(f, converter)), convertDepthFirst(a, converter)))
      case Conditioned(t, c) => converter.convert(Conditioned(convertDepthFirst(t, converter), c))
      case SeqTerm(args) => converter.convert(SeqTerm(args.map(convertDepthFirst(_, converter))))
      case ImageSeq(f) => converter.convert(ImageSeq(FunTerm(convertDepthFirst(f, converter))))
      case ImageSeqCurried2(f) => converter.convert(ImageSeqCurried2(FunTerm(convertDepthFirst(f, converter))))
      case LambdaAbstraction(Var(v, d), t) => converter.convert(LambdaAbstraction(Var(v, convertDepthFirst(d, converter)), convertDepthFirst(t, converter)))
      case Var(v, d) => converter.convert(Var(v, convertDepthFirst(d, converter)))
      case Reduce(o, a) => converter.convert(Reduce(FunTerm(convertDepthFirst(o, converter)).asInstanceOf[FunTerm[(T, T), T]], convertDepthFirst(a, converter)))
      case _ => converter.convert(term)
    }
  }

  def substituteVariable[A, B](term: Term[A], variable: Variable[B], replacement: Term[B]) =
    convertDepthFirst(term, new Converter {
      def convert[T](term: Term[T]) = term match {
        case v: Variable[Any] if v == variable => replacement.asInstanceOf[Term[T]]
        case _ => term
      }
    })

  def distConds[T](term: Term[T]): Term[T] = {
    implicit def cast(t: Term[Any]) = t.asInstanceOf[Term[T]]
    term match {
      case Conditioned(const@Constant(_), _) => const
      case Conditioned(LinearModel(feats, weights, base), c) => LinearModel(distConds(feats | c), weights, distConds(base | c))
      case Conditioned(Math.VecAdd.Reduced(args), c) => vsum(distConds(args | c))
      case Conditioned(Math.VecAdd.Applied2(arg1, arg2), c) => distConds(arg1 | c) + distConds(arg2 | c)
      case Conditioned(Math.Dot.Applied2(arg1, arg2), c) => distConds(arg1 | c) dot distConds(arg2 | c)
      case Conditioned(Math.DoubleAdd.Reduced(args), c) => dsum(distConds(args | c))
      case Conditioned(Math.DoubleAdd.Applied2(arg1, arg2), c) => distConds(arg1 | c) + distConds(arg2 | c)
      case Conditioned(SeqTerm(args), c) => SeqTerm(args.map(a => distConds(a | c)))
      case Conditioned(ImageSeq(LambdaAbstraction(Var(v, d), arg)), c) => ImageSeq(LambdaAbstraction(Var(v, distConds(d | c)), distConds(arg | c)))
      case _ => term
    }
  }

  def flattenDoubleSums(term: Term[Double]): Seq[Term[Double]] = {
    val raw = term match {
      case Math.DoubleAdd.Reduced(SeqTerm(args)) => args.flatMap(flattenDoubleSums)
      case Math.DoubleAdd.Applied2(arg1, arg2) => flattenDoubleSums(arg1) ++ flattenDoubleSums(arg2)
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
      case f@FunApp(fun, arg) => FunApp(FunTerm(flattenDouble(fun)), flattenDouble(arg))
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
      case r@Reduce(op, args) => r.copy(op = unrollLambdas(op).asInstanceOf[FunTerm[(T, T), T]], arguments = unrollLambdas(args))
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
      case Conditioned(t, c) => Conditioned(distDots(t), c)
      case LinearModel(f, w, base) => distDots(f dot w) + base
      case Math.Dot.Applied2(Math.VecAdd.Applied2(f1, f2), w) => distDots(f1 dot w) + distDots(f2 dot w)
      case Math.Dot.Applied2(Math.VecAdd.Reduced(SeqTerm(args)), w) => dsum(SeqTerm(args.map(a => distDots(a dot w))))
      case Math.Dot.Applied2(Quantified.VecSum(ImageSeq(LambdaAbstraction(v, arg))), w) =>
        dsum(LambdaAbstraction(v, distDots(arg dot w)))
      case _ => term
    }
  }

  def simplifyConds[T](term: Term[T]): Term[T] = {
    term match {
      case Conditioned(Conditioned(t, c1), c2) => Conditioned(t, c1 + c2)
      case app@FunApp(f, a) => app.copy(FunTerm(simplifyConds(f)), simplifyConds(a))
      case _ => term
    }
  }

  def groupLambdas[T](term: Term[T]): Term[T] = {
    term match {
      case Math.VecAdd.Reduced(SeqTerm(args)) =>
      //val lambdas = args.collect({case l@LambdaAbstraction(_, _) => l})
      //find terms with the same set of hidden variables (with respect to the quantified variable)
      //for that: (1) find FunApps of predicates (2) check whether they have identical structure
      //(same arguments with variables in same domain)
    }
    ???
  }

  def mergeLambdas[T](t1: Term[T], t2: Term[T],
                      merger: (Term[Any], Term[Any]) => Term[Any], condition: State = State.empty): Option[Term[T]] = {
    implicit def cast(t: Term[Any]) = t.asInstanceOf[Term[T]]
    //two lambda abstractions can be merged if they have the same domain, and for each element of the domain
    //the set of free variables in both function terms is identical for each binding
    //we approximate this as follows:
    //  take a default element of the variable domain, and pass it into both function terms
    //  if the variables of these "conditioned" terms are identical, we can merge
    //  in this case we can try to further merge the conditioned terms in case they contain additional lambda abstractions
    (t1, t2) match {
      case (l1@LambdaAbstraction(v1@Var(_, d1), a1), l2@LambdaAbstraction(v2@Var(_, d2), a2)) if d1 == d2 =>
        val default = d1.default.head
        val newVar = v1
        val replaced1 = a1
        val replaced2 = substituteVariable(a2, v2, v1)
        val newCondition = condition + SingletonState(newVar, default)
        val conditioned1 = replaced1 | newCondition
        val conditioned2 = replaced2 | newCondition
        val vars1 = conditioned1.variables
        val vars2 = conditioned2.variables
        if (vars1 == vars2) {
          mergeLambdas(replaced1, replaced2, merger, newCondition) match {
            case Some(merged) => Some(LambdaAbstraction(newVar, merged))
            case None => Some(LambdaAbstraction(newVar, merger(replaced1, replaced2)))
          }
        } else None
      case _ => None
    }
  }

  def main(args: Array[String]) {

    val r = 'r of (0 ~~ 2 |-> Doubles)
    val l1 = for (i <- 0 ~~ 2) yield r(i)
    val l2 = for (j <- 0 ~~ 2) yield r(j)

    val merged = mergeLambdas(l1,l2,(t1:Term[Any],t2:Term[Any]) => t1.asInstanceOf[Term[Double]] + t2.asInstanceOf[Term[Double]])

    println(merged)
  }


}


