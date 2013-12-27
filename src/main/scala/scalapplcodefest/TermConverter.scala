package scalapplcodefest

import scala.language.implicitConversions
import scalapplcodefest.value._
import scalapplcodefest.term._
import scala.Some
import scalapplcodefest.term.ImageSeq1
import scalapplcodefest.value.Reduce
import scalapplcodefest.term.ImageSeq2
import scalapplcodefest.term.LambdaAbstraction
import scalapplcodefest.term.SeqTerm
import scalapplcodefest.term.TupleTerm2
import scalapplcodefest.term.FunApp

/**
 * A set of methods to convert one term into another.
 *
 * @author Sebastian Riedel
 */
object TermConverter {

  import TermDSL._

  /**
   * A term converter converts a single term, usually only the root of the term.
   * Full tree conversion can be achieved by combining a converter with the
   * convertDepthFirst method.
   */
  trait Converter {
    def convert[A](toConvert: Term[A]): Term[A]
  }

  /**
   * This method converts the term tree in a depth-first fashion. That is, when
   * encountering a term the term's children are converted, a new term with these converted children is created,
   * and then this new term is converted.
   * @param term the term to convert.
   * @param converter a converter that takes an individual term and converts it (no traversal).
   * @tparam T the type of term.
   * @return the converted term.
   */
  def convertChildrenFirst[T](term: Term[T], keepBrackets: Boolean = true)(converter: Converter): Term[T] = {
    def convert[A](term: Term[A]) = converter.convert(term)
    def ccf[A](term: Term[A]) = convertChildrenFirst(term, keepBrackets)(converter)
    implicit def cast(t: Term[Any]) = t.asInstanceOf[Term[T]]
    term match {
      case SeqTerm(args) => convert(SeqTerm(args.map(ccf)))
      case c: Composite1[_, _] => convert(c.asAny.copy(ccf(c.components)))
      case c: Composite2[_, _, _] => convert(c.asAny.copy(ccf(c.components._1), ccf(c.components._2)))
      case c: Composite3[_, _, _, _] => convert(c.asAny.copy(ccf(c.components._1), ccf(c.components._2), ccf(c.components._3)))
      case _ => convert(term)
    }
  }

  /**
   * This method converts the term tree by converting parents first, and then its children.
   * @param term the term to convert.
   * @param converter a converter that takes an individual term and converts it (no traversal).
   * @tparam T the type of term.
   * @return the converted term.
   */
  def convertParentFirst[T](term: Term[T])(converter: Converter): Term[T] = {
    def convert[A](term: Term[A]) = converter.convert(term)
    def cpf[A](term: Term[A]) = convertParentFirst(term)(converter)
    implicit def cast(t: Term[Any]) = t.asInstanceOf[Term[T]]
    convert(term) match {
      case SeqTerm(args) => SeqTerm(args.map(cpf))
      case c: Composite1[_, _] => c.asAny.copy(cpf(c.components))
      case c: Composite2[_, _, _] => c.asAny.copy(cpf(c.components._1), cpf(c.components._2))
      case c: Composite3[_, _, _, _] => c.asAny.copy(cpf(c.components._1), cpf(c.components._2), cpf(c.components._3))
      case _ => term
    }
  }

  /**
   * Collects a list of items by traversing through the term tree.
   * @param term the term tree to traverse.
   * @param collector a function to collect items for a single node
   * @tparam C type of items.
   * @return list of all items collected from term tree.
   */
  def collect[C](term:Term[Any], collector:Term[Any] => List[C]):List[C] = {
    term match {
      case c:Composite[_] => collector(c) ++ c.componentSeq.toList.flatMap(collect(_,collector))
      case _ => collector(term)
    }
  }

  /**
   * Converts a term by replacing each occurrence of a sub-term with another term.
   * @param term the term in which the replacement should happen
   * @param toReplace the inner term that should be replaced.
   * @param replacement the new term to use instead.
   * @tparam A type parameter of root term to change.
   * @tparam B type parameter of term to replace.
   * @return the original term with all occurrences of `toReplace` replaced by `replacement`.
   */
  def substituteTerm[A, B](term: Term[A], toReplace: Term[B], replacement: Term[B]) = convertChildrenFirst(term) {
    new Converter {
      def convert[T](term: Term[T]) = if (term == toReplace) replacement.asInstanceOf[Term[T]] else term
    }
  }

  def groundSig[A, B](term: Term[A], toReplace: Sig[B], replacement: B): Term[A] = {
    (toReplace, replacement) match {
      case (VarSig(v), value) => substituteTerm(term, v, Constant(value))
      case (TupleSig2(s1, s2), (v1, v2)) => groundSig(groundSig(term, s1, v1), s2, v2)
      case (TupleSig3(s1, s2, s3), (v1, v2, v3)) => groundSig(groundSig(groundSig(term, s1, v1), s2, v2), s3, v3)
      case _ => term
    }
  }

  def replaceSig[A, B](term: Term[A], toReplace: Sig[B], replacement: Sig[B]): Term[A] = {
    (toReplace, replacement) match {
      case (VarSig(a), VarSig(b)) => substituteTerm(term, a, b)
      case (TupleSig2(a1, a2), TupleSig2(b1, b2)) => groundSig(groundSig(term, a1, b1), a2, b2)
      case (TupleSig3(a1, a2, a3), TupleSig3(b1, b2, b3)) => groundSig(groundSig(groundSig(term, a1, b1), a2, b2), a3, b3)
      case _ => term
    }
  }


  /**
   * Replaces variables with Constants according to the value assigned to the variable in the given state.
   * @param term the term to convert.
   * @param state the binding of variables to values used to replace variables.
   * @tparam T the type of term to convert.
   * @return a term in which variables are replaced with constants if the variables have bindings in the state.
   */
  def ground[T](term: Term[T], state: State) = convertChildrenFirst(term) {
    new Converter {
      def convert[A](term: Term[A]) = term match {
        case v: Variable[_] => state.get(v).map(value => Constant(value)).getOrElse(v).asInstanceOf[Term[A]]
        case _ => term
      }
    }
  }

  /**
   * This function takes conditioned terms within the term tree and moves the condition downward as far as possible.
   * @param term the term to convert.
   * @tparam T type of term
   * @return a term in which conditioned terms are to be found at the leaf variables and predicate applications.
   */
  def pushDownConditions[T](term: Term[T]): Term[T] = {
    def pushConditions[S](term: Term[S], condition: State): Term[S] = convertChildrenFirst(term) {
      new Converter {
        def convert[A](arg: Term[A]) = arg match {
          case v@Var(_, _) if condition.domain(v) => Conditioned(v, condition)
          case a@GroundAtom(_, _) if condition.domain(a) => Conditioned(a, condition)
          case f@FunApp(Predicate(_, _, _), _) if !condition.domain.isEmpty => Conditioned(f, condition) //todo: should check whether predicate is involved
          case t => t
        }
      }
    }
    def findAndPushConditions[S](term: Term[S]) = convertChildrenFirst(term) {
      new Converter {
        def convert[A](arg: Term[A]) = arg match {
          case Conditioned(t, state) => pushConditions(t, state)
          case t => t
        }
      }
    }
    findAndPushConditions(term)
  }

  /**
   * Convert images of lambda abstractions to sequences of terms by replacing the lambda variables with
   * constant values in the variable domain.
   * @param term the term to convert
   * @tparam T type of term to convert
   * @return the term with all images of lambda abstractions replaced with sequences of terms.
   */
  def unrollLambdaImages[T](term: Term[T], wrap: Term[Any] => Term[Any] = identity) = convertChildrenFirst(term) {
    new Converter {
      def convert[A](term: Term[A]) = term match {
        case ImageSeq1(LambdaAbstraction(sig, t)) =>
          val domainSeq = sig.domain.eval().get.view.toSeq
          SeqTerm(domainSeq.map(value => wrap(groundSig(t, sig, value)))).asInstanceOf[Term[A]]
        case t => t
      }
    }
  }


  /**
   * Replaces trees of applications of binary operators with reductions of the operator to the sequence of terms on
   * the tree's yield.
   * @param term the term to convert.
   * @param op the binary operator to flatten trees for
   * @tparam T type of term to convert
   * @tparam O type of arguments to the binary operator
   * @return term with flattened trees.
   */
  def flatten[T, O](term: Term[T], op: ConstantOperator[O]): Term[T] = convertChildrenFirst(term) {
    new Converter {
      def convert[A](arg: Term[A]) = {
        implicit def cast(t: Term[Any]) = t.asInstanceOf[Term[A]]
        arg match {
          case op.Reduced(SeqTerm(args)) =>
            val inner = args.collect({
              case op.Reduced(SeqTerm(innerArgs)) => innerArgs
              case t => Seq(t)
            })
            val flattened = inner.flatMap(identity)
            Reduce(op, SeqTerm(flattened))
          case op.Applied2(op.Reduced(k@SeqTerm(args1)), op.Reduced(SeqTerm(args2))) =>
            Reduce(op, SeqTerm(args1 ++ args2))
          case op.Applied2(op.Reduced(SeqTerm(args1)), arg2) =>
            Reduce(op, SeqTerm(args1 :+ arg2))
          case op.Applied2(arg1, op.Reduced(SeqTerm(args2))) =>
            Reduce(op, SeqTerm(arg1 +: args2))
          case op.Applied2(arg1, arg2) =>
            Reduce(op, SeqTerm(Seq(arg1, arg2)))
          case _ => arg

        }
      }
    }
  }


  /**
   * Pushes dot products into sums of doubles
   * @param term the term to convert.
   * @tparam T type of term to convert.
   * @return a term where dot products of vector sums are replaced by double sums of dot products.
   */
  def pushDownDotProducts[T](term: Term[T]): Term[T] = convertChildrenFirst(term) {
    new Converter {
      def convert[A](term: Term[A]) = {
        val dots = term match {
          case vectors.dot.Applied2(vectors.add.Reduced(ImageSeq1(LambdaAbstraction(v, t1))), a2) =>
            Seq(doubles.sumSeq(ImageSeq1(LambdaAbstraction(v, t1 dot a2))))
          case vectors.dot.Applied2(vectors.add.Reduced(SeqTerm(args1)), vectors.add.Reduced(SeqTerm(args2))) =>
            for (a1 <- args1; a2 <- args2) yield a1 dot a2
          case vectors.dot.Applied2(a1, vectors.add.Reduced(SeqTerm(args2))) =>
            for (a2 <- args2) yield a1 dot a2
          case vectors.dot.Applied2(vectors.add.Reduced(SeqTerm(args1)), a2) =>
            for (a1 <- args1) yield a1 dot a2
          case _ => Seq.empty
        }

        val pushedDown = dots.map(pushDownDotProducts(_))

        pushedDown match {
          case s if s.size == 1 => s(0).asInstanceOf[Term[A]]
          case s if s.size > 1 => doubles.sumSeq(SeqTerm(s)).asInstanceOf[Term[A]]
          case _ => term
        }
      }
    }
  }

  /**
   * Finds sums of lambda abstractions and turns them into lambda abstractions of sums. Two lambda abstractions
   * are merged / grouped when they have the same argument domain and are likely to produce the same free variables.
   * @param term term to convert.
   * @param filter decides which hidden variables should be used as criteria for merging. Two lambda abstractions will
   *               be merged if, when applied to the first element of the domain, both abstractions
   *               give the same free variables after applying the filter.
   * @tparam T type of term to convert.
   * @return term in which sums of lambda abstractions have been converted to lambda abstractions of sums.
   */
  def groupLambdas[T](term: Term[T], filter: Variable[Any] => Boolean = x => true) = convertChildrenFirst(term) {
    new Converter {
      def convert[A](arg: Term[A]) = groupLambdasOnce(arg, filter)
    }
  }

  /**
   * Unwrap bracketed terms in the tree
   * @param root the term to convert.
   * @tparam T type parameter of the term to convert.
   * @return a term tree in which each bracketed term has been replaced with the term in brackets.
   */
  def unbracket[T](root: Term[T]) = convertChildrenFirst(root, false) {
    new Converter {
      def convert[A](term: Term[A]) = term match {
        case Bracketed(t) => t
        case _ => term
      }
    }
  }

  def asSeq[T](term: Term[T], op: ConstantOperator[T]): Seq[Term[T]] = term match {
    case op.Reduced(SeqTerm(args)) => args
    case _ => Seq(term)
  }

  /**
   * Wraps brackets around the term inside lambda abstractions.
   * @param root the term to convert.
   * @tparam T type of term to convert.
   * @return term with terms inside lambda abstractions bracketed.
   */
  def bracketInsideLambda[T](root: Term[T]) = convertChildrenFirst(root) {
    new Converter {
      def convert[A](term: Term[A]) = term match {
        case LambdaAbstraction(v, t) => LambdaAbstraction(v, Bracketed(t)).asInstanceOf[Term[A]]
        case _ => term
      }
    }
  }

  def groupLambdasOnce[T](term: Term[T], filter: Variable[Any] => Boolean = x => true): Term[T] = {
    implicit def cast(t: Term[Any]) = t.asInstanceOf[Term[T]]

    //merge a term with one element in a list of previous terms if possible, otherwise prepend
    def mergeOneTerm[A](current: Seq[Term[A]], toMerge: Term[A], op: ConstantOperator[A]) = {
      val first = current.view.map(that => that -> mergeLambdas(that, toMerge, op, filter = filter)).find(_._2.isDefined)
      first match {
        case Some((orig, Some(mergedTerm))) =>
          val mapped = current.map(t => if (t == orig) mergedTerm else t)
          mapped
        case _ => current :+ toMerge
      }
    }
    term match {
      case vectors.add.Reduced(SeqTerm(args)) =>
        val merged = args.foldLeft(Seq.empty[Term[Vector]])(mergeOneTerm(_, _, vectors.add))
        if (merged.size > 1) vectors.sumSeq(SeqTerm(merged)) else merged(0)
      case doubles.add.Reduced(SeqTerm(args)) =>
        val merged = args.foldLeft(Seq.empty[Term[Double]])(mergeOneTerm(_, _, doubles.add))
        if (merged.size > 1) doubles.sumSeq(SeqTerm(merged)) else merged(0)
      case _ => term
    }
  }

  def mergeLambdas[T](t1: Term[T], t2: Term[T],
                      operator: ConstantOperator[T],
                      condition: State = State.empty,
                      filter: Variable[Any] => Boolean = x => true): Option[Term[T]] = {

    def mergeable(replaced1: Term[T], replaced2: Term[T], newCondition: State) = {
      val conditioned1 = ground(replaced1, newCondition)
      val conditioned2 = ground(replaced2, newCondition)
      val vars1 = conditioned1.variables.filter(filter)
      val vars2 = conditioned2.variables.filter(filter)
      vars1 == vars2
    }

    (t1, t2) match {
      case (operator.Reduced(ImageSeq1(LambdaAbstraction(v1, a1))), operator.Reduced(ImageSeq1(LambdaAbstraction(v2, a2)))) =>
        val default = v1.default
        val grounded1 = groundSig(a1, v1, default)
        val grounded2 = groundSig(a2, v2, default) //substituteTerm(a2, v2, v1)
        if (mergeable(grounded1, grounded2, condition)) {
          val replaced = replaceSig(a2, v2, v1)
          Some(Reduce(operator, ImageSeq1(LambdaAbstraction(v1, operator.reduce(SeqTerm(Seq(a1, replaced)))))))
        } else
          None
      case _ => None
    }

  }


}


