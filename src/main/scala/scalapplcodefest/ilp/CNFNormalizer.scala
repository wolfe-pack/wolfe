package scalapplcodefest.ilp

import scalapplcodefest.TermDSL._
import scalapplcodefest.TermConverter._
import scalapplcodefest.term._
import scalapplcodefest.MPGraphCompiler.factorize
import scalapplcodefest.value.Bools.Implies
import scalapplcodefest.value.Vectors.Dot
import scalapplcodefest.value.{Reduce, Fun}
import scalapplcodefest.value.Reduce
import scalapplcodefest.term.Var
import scalapplcodefest.term.TupleTerm2
import scalapplcodefest.term.FunApp

/**
 * User: rockt
 * Date: 12/29/13
 * Time: 11:00 AM
 */

object CNFNormalizer {
  def apply[T](base: Term[T], op: ConstantOperator[T]): Term[T] = {
    def normalize(term: Term[_]): Term[_] = term match {
      case FunApp(bools.implies, TupleTerm2(left, right)) =>
        println("Found it!")
        term
//      case FunApp(vectors.dot, TupleTerm2(left, right)) =>
//        FunApp(vectors.dot.asInstanceOf[Term[Fun[Any, Any]]], TupleTerm2(normalize(left), normalize(right)))
//      case FunApp(vectors.add, TupleTerm2(left, right)) =>
//        FunApp(vectors.add.asInstanceOf[Term[Fun[Any, Any]]], TupleTerm2(normalize(left), normalize(right)))
      case Reduce(operator, arguments) =>
        Reduce(operator.asInstanceOf[Term[Fun[(Any, Any), Any]]], normalize(arguments).asInstanceOf[Term[Seq[Any]]])
      case SeqTerm(seq) => SeqTerm(seq map normalize)
      case Var(_, _) => term
      case FunApp(operator, TupleTerm2(left, right)) => FunApp(operator, TupleTerm2(normalize(left), normalize(right)))
      case FunApp(operator, inner) => FunApp(operator, normalize(inner))
      case _ =>
        println("TODO: " + term.getClass)
        term
    }

    val factorizedTerm = factorize[T](base, op)

    println("Seq:")
    println(factorizedTerm mkString "\n")

    val normalized = factorizedTerm map normalize

    normalized.head.asInstanceOf[Term[T]]
  }
}