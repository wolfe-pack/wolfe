package scalapplcodefest.ilp

import scalapplcodefest.TermDSL._
import scalapplcodefest.term._
import scalapplcodefest.MPGraphCompiler.factorize
import scalapplcodefest.value.Fun
import scalapplcodefest.term.Var
import scalapplcodefest.term.TupleTerm2
import scalapplcodefest.term.FunApp

/**
 * Transformes all formulae to CNF by factorizing (unrolling) them beforehand.
 */

object CNFNormalizer {
  def apply[T](base: Term[T], op: ConstantOperator[T]): Term[T] = {
    val myImplies = bools.implies.asInstanceOf[Term[Fun[Any, Boolean]]]
    val myAnd = bools.and.asInstanceOf[Term[Fun[Any, Boolean]]]
    val myOr = bools.or.asInstanceOf[Term[Fun[Any, Boolean]]]
    val myNeg = bools.neg.asInstanceOf[Term[Fun[Any, Boolean]]]

    // recursive method for normalization
    def normalize(term: Term[_]): Term[_] = {
      /**
       * Checks if formula is normalized.
       *
       * @param term given factorized formula
       * @return
       */
      def checkIfNormalized(term: Term[_]): Boolean = {
        def checkForOr(term: Term[_]): Boolean = {
          term match {
            case FunApp(bools.and | bools.equiv | bools.implies, _) =>
              false
            case FunApp(bools.or, TupleTerm2(left, right)) =>
              checkForOr(left) && checkForOr(right)
            case predCon@(Predicate(_, _, _) | Constant(_)) => true
            case FunApp(bools.neg, predCon) => true
            case FunApp(Predicate(_, _, _), t) => true
            case FunApp(Constant(_), t) => true
            case _ =>
              false
          }
        }
        term match {
          case FunApp(bools.and, TupleTerm2(left, right)) => checkForOr(left) && checkForOr(right)
          case _ => checkForOr(term)
        }
      }

      /**
       * Transforms a factorized (unrolled, etc.) formula to CNF.
       *
       * @param term
       * @return
       */
      def normalizeFormula(term: Term[_]): Term[_] = term match {
        //If φ has the form P -> Q, then:
        //Return CONVERT(~P v Q). // equivalent
        case FunApp(bools.implies, TupleTerm2(left, right)) =>
          normalizeFormula(FunApp(myOr, TupleTerm2(FunApp(myNeg, left), right)))
        //If φ has the form P <-> Q, then: return CONVERT((P ^ Q) v (~P ^ ~Q)).
        case FunApp(bools.equiv, TupleTerm2(left, right)) =>
          normalizeFormula(FunApp(myAnd, TupleTerm2(
            FunApp(myImplies, TupleTerm2(left, right)),
            FunApp(myImplies, TupleTerm2(right, left))
          )))
        // If φ has the form ~(~P), then return CONVERT(P). // double negation
        case FunApp(bools.neg, FunApp(bools.neg, expr)) =>
          normalizeFormula(expr)
        // If φ has the form ~(P ^ Q), then return CONVERT(~P v ~Q). // de Morgan's Law
        case FunApp(bools.neg, FunApp(bools.and, TupleTerm2(left, right))) =>
          normalizeFormula(FunApp(myOr, TupleTerm2(FunApp(myNeg, left), FunApp(myNeg, right))))
        // If φ has the form ~(P v Q), then return CONVERT(~P ^ ~Q). // de Morgan's Law
        case FunApp(bools.neg, FunApp(bools.or, TupleTerm2(left, right))) =>
          normalizeFormula(FunApp(myAnd, TupleTerm2(FunApp(myNeg, left), FunApp(myNeg, right))))
        //  If φ has the form (P1 ^ P2)  v Q, then:
        //  return CONVERT((P1 v Q) ^ (P2 v Q))
        case FunApp(bools.or, TupleTerm2(FunApp(bools.and, TupleTerm2(left, right)), expr)) =>
          normalizeFormula(FunApp(myAnd, TupleTerm2(FunApp(myOr, TupleTerm2(left, expr)), FunApp(myOr, TupleTerm2(right, expr)))))
        //  If φ has the form P v (Q1 ^ Q2), then:
        //  return CONVERT((P v Q1) ^ (P v Q2))
        case FunApp(bools.or, TupleTerm2(expr, FunApp(bools.and, TupleTerm2(left, right)))) =>
          normalizeFormula(FunApp(myAnd, TupleTerm2(FunApp(myOr, TupleTerm2(expr, left)), FunApp(myOr, TupleTerm2(expr, right)))))
        //  If φ has the form P v Q, then:
        //  return CONVERT(P) v CONVERT(Q)
        case FunApp(bools.or, TupleTerm2(left, right)) =>
          FunApp(myOr, TupleTerm2(normalizeFormula(left), normalizeFormula(right)))
        //  If φ has the form P ^ Q, then:
        //  return CONVERT(P) ^ CONVERT(Q)
        case FunApp(bools.and, TupleTerm2(left, right)) =>
          FunApp(myAnd, TupleTerm2(normalizeFormula(left), normalizeFormula(right)))
        // If φ is a constant, then: return φ.
        case conpred@(Constant(_) | Predicate(_, _, _)) => term
        case FunApp(conpred, _) => term
        // If φ has the form ~A for some constant A, then return φ.
        //case FunApp(bools.neg, conpred) =>
        //  term
        case _ =>
          println("not yet covered " + term + " class " + term.getClass)
          term
      }
      term match{
        // dig to the actual formulas
        case Reduce(operator, arguments) =>
          Reduce(operator.asInstanceOf[Term[Fun[(Any, Any), Any]]], normalize(arguments).asInstanceOf[Term[Seq[Any]]])
        case SeqTerm(seq) => SeqTerm(seq map normalize)
        case Var(_, _) => term
        case FunApp(ops @(bools.equiv | bools.implies | bools.or | bools.and | bools.neg),_) =>
        var normalizedTerm = normalizeFormula(term)
          //println("normalized " + normalizedTerm + " isNormalized " + checkIfNormalized(normalizedTerm))
          while(!checkIfNormalized(normalizedTerm)){
            normalizedTerm = normalizeFormula(normalizedTerm)
            //println("normalized " + normalizedTerm + " isNormalized " + checkIfNormalized(normalizedTerm))
          }
          normalizedTerm
        case Predicate(_,_,_) => term
        case Constant(_) =>  term
        case FunApp(operator, inner) => //if ((operator != bools.equiv) && (operator != bools.implies) && (operator != bools.or) && (operator != bools.and) && (operator != bools.neg))
          inner match {
            case TupleTerm2(left, right) => FunApp(operator, TupleTerm2(normalize(left), normalize(right)))
            case _ => FunApp(operator, normalize(inner))
          }
        case _ =>
          println("not yet covered: " + term + " class " + term.getClass)
          term
      }
    }
    // factorize term
    val factorizedTerm = factorize[T](base, op)
    // normalize term
    val normalized = factorizedTerm map normalize
    normalized.head.asInstanceOf[Term[T]]
  }
}