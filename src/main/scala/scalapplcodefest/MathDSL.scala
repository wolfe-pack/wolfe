package scalapplcodefest

import scalapplcodefest.term._
import scalapplcodefest.value.Fun
import scalapplcodefest.term.LambdaAbstraction
import scalapplcodefest.term.Reduce

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


    //val is = Σ(for (i <- 0 ~~ n) yield i)
    //println(is.value(n -> 3))

    def doMagic(a: Array[_]) = a match {
      case b: Array[Int] => b.head
      case b: Array[Double] => b.last
      case b: Array[String] => "blub"
    }

    println(doMagic(Array(1, 2, 3)))
    println(doMagic(Array(1.0, 2.0, 3.0)))
    println(doMagic(Array("abc", "efg")))
    
    def doWondersBuggy(a: Array[(_, _)]) = a match {
      case b: Array[(_, Int)] => b.head._2
      case b: Array[(_, Double)] => b.last._2
      case b: Array[(_, String)] => "blub"
    }

    println(doWondersBuggy(Array("a" -> 1, "b" -> 2, "c" -> 3)))
    println(doWondersBuggy(Array("a" -> 1.0, "b" -> 2.0, "c" -> 3.0)))
    println(doWondersBuggy(Array("a" -> "abc", "b" -> "def")))

    import reflect._
    class Def[C](implicit desired : Manifest[C]) {
      def unapply[X](c : X)(implicit m : Manifest[X]) : Option[C] = {
        def sameArgs = desired.typeArguments.zip(m.typeArguments).forall { case (desired, actual) => desired >:> actual }
        if (desired >:> m && sameArgs) Some(c.asInstanceOf[C])
        else None
      }
    }

    val IntTupleArray = new Def[Array[(_, Int)]]
    val DoubleTupleArray = new Def[Array[(_, Double)]]
    val StringTupleArray = new Def[Array[(_, String)]]

    def doWonders[A](a: A)(implicit m: Manifest[A]) = a match {
      case IntTupleArray(b) => b.head._2
      case DoubleTupleArray(b) => b.last._2
      case StringTupleArray(b) => "blub"
      case _ => println("I don't know yet what to do with: " + a)
    }

    println(doWonders(Array("a" -> 1, "b" -> 2, "c" -> 3)))
    println(doWonders(Array("a" -> 1.0, "b" -> 2.0, "c" -> 3.0)))
    println(doWonders(Array("a" -> "abc", "b" -> "def")))
  }
}