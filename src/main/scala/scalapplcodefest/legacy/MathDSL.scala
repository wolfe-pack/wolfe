package scalapplcodefest.legacy

import scalapplcodefest.legacy.term._
import scalapplcodefest.legacy.value.Fun
import scalapplcodefest.legacy.term.LambdaAbstraction
import scalapplcodefest._
import scala.Some
import scalapplcodefest.legacy.term.FunApp

/**
 * User: rockt
 * Date: 12/30/13
 * Time: 6:14 PM
 */

object MathDSL {
  import TermDSL._

  def λ[A, B](sig: Sig[A])(body: Term[B]) = LambdaAbstraction(sig, body)

  implicit def toRichVecPimped(term: Term[Vector]) = RichVecTermPimped(RichVecTerm(term))

  case class RichVecTermPimped(richVecTerm: RichVecTerm) {
    def ∘(that: Term[Vector]) = richVecTerm.dot(that)
  }

  import reflect._
  /**
   * From http://daily-scala.blogspot.co.uk/2010/01/overcoming-type-erasure-in-matching-1.html
   */
  class Def[C](implicit desired: Manifest[C]) {
    def unapply[X](c: X)(implicit m: Manifest[X]): Option[C] = {
      def sameArgs = desired.typeArguments.zip(m.typeArguments).forall { case (desired, actual) => desired >:> actual }
      if (desired >:> m && sameArgs) Some(c.asInstanceOf[C])
      else None
    }
  }

  def Σ[F: Manifest, T: Manifest](seq: Seq[FunApp[F,T]]) = {
    val SeqToVector = new Def[Seq[FunApp[F, Vector]]]
    val SeqToDouble = new Def[Seq[FunApp[F, Double]]]
    val SeqToInt = new Def[Seq[FunApp[F, Int]]]
    (seq match {
      case SeqToVector(s) => vectors.sumSeq(s)
      case SeqToDouble(s) => doubles.sumSeq(s)
      case SeqToInt(s) => ints.sumSeq(s)
      //case _ => throw new NotImplementedError(s"I don't know yet how sum over a $seq")
    }).asInstanceOf[Term[T]]
  }

  def Σ[F: Manifest, T: Manifest](fun: Term[Fun[F,T]]) = {
    val TermFunToVector = new Def[Term[Fun[F, Vector]]]
    val TermFunToDouble = new Def[Term[Fun[F, Double]]]
    val TermFunToInt = new Def[Term[Fun[F, Int]]]
    (fun match {
      case TermFunToVector(t) => vectors.sum(t)
      case TermFunToDouble(t) => doubles.sum(t)
      case TermFunToInt(t) => ints.sum(t)
      //case _ => throw new NotImplementedError(s"I don't know yet how sum over a $fun")
    }).asInstanceOf[Term[T]]
  }

  def main(args: Array[String]) {
    val n = 'n of ints



    val f1 = λ(n){
      val tmp = n + 1
      tmp * tmp
    }

    println(f1.value()(3))

    val vecSum = Σ(for (i <- 0 ~~ n) yield unit(i))

    println("vecSum: " + vecSum)

    val θ = 'θ of vectors
    val feat = 'feat of vectors

    val model = θ ∘ feat

    val intSum = Σ(for (i <- 0 ~~ n) yield i)

    println("intSum: " + intSum)
    println(intSum.value(n -> 10) + 5)

    val lam = λ(n)(Σ(for (i <- 0 ~~ n) yield i) + n)
    println(lam.value()(5))
 }
}