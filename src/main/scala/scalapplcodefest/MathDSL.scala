package scalapplcodefest

import scalapplcodefest.term._
import scalapplcodefest.value.Fun
import scalapplcodefest.term.LambdaAbstraction

/**
 * User: rockt
 * Date: 12/30/13
 * Time: 6:14 PM
 */

object MathDSL {
  import TermDSL._
  import TermDebugger._

  object λ {
    def apply[A, B](sig: Sig[A])(body: Term[B]) = LambdaAbstraction(sig, body)
  }

  def Σ(args: Term[Vector]*) = vectors.sum(args: _*)
  def Σ[A](args: Term[Fun[A, Vector]]) = vectors.sum(args)

  implicit def toRichVecPimped(term: Term[Vector]) = RichVecTermPimped(RichVecTerm(term))

  case class RichVecTermPimped(richVecTerm: RichVecTerm) {
    def ∘(that: Term[Vector]) = richVecTerm.dot(that)
  }

  def main(args: Array[String]) {
    val n = 'n of ints

    val f1 = λ(n){
      val tmp = n + 1
      tmp * tmp
    }
    println(termToPrettyString(f1))

    Σ(for (i <- 0 ~~ n) yield unit(i))

    val θ = 'θ of vectors
    val feat = 'feat of vectors

    val model = θ ∘ feat
  }
}