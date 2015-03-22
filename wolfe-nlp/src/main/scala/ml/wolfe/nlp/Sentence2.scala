package ml.wolfe.nlp

import scala.collection.generic.CanBuildFrom
import scala.collection.mutable
import scala.language.implicitConversions

/**
 * @author Ingolf Becker
 * Sentence2 is a class that offers traversable operations to any underlying Token that is based on a BaseToken.
 */


abstract class Sentence2Base[A <: BaseTokenTest] {
  val tokens: IndexedSeq[A]
}

case class Sentence2[A <: BaseTokenTest with TokenWithStringLike](tokens: IndexedSeq[A]) extends Sentence2Base[A]
                                                                                   with SentenceWithStringLike[A]

object Sentence2 {


  implicit def fromString[A <: BaseTokenTest with TokenWithStringLike](str: String)
                    (implicit cbf: CanBuildFrom[String, A, Sentence2[A]]) : Sentence2[A] = cbf.apply(str).result()

  implicit def canBuildFrom[A <: BaseTokenTest with TokenWithStringLike]:
                            CanBuildFrom[String, Sentence2[A], Document2[A,Sentence2[A]]]  = {
    new CanBuildFrom[String, Sentence2[A], Document2[A,Sentence2[A]]] {
      def newBuilder: mutable.Builder[Sentence2[A], Document2[A, Sentence2[A]]] = {
        new mutable.ArrayBuffer[Sentence2[A]] mapResult( t=> Document2[A, Sentence2[A]](t.toIndexedSeq))
      }
      override def apply(from: String): mutable.Builder[Sentence2[A], Document2[A, Sentence2[A]]] = newBuilder
      override def apply(): mutable.Builder[Sentence2[A], Document2[A, Sentence2[A]]] = newBuilder
    }
  }
}

trait SentenceWithStringLike[A <: BaseTokenTest with TokenWithStringLike] {
  this: Sentence2Base[A] =>
  def toPrettyString = tokens.map(_.toPrettyString) mkString " "
}

trait SentenceWithTaggedTextLike {
  this: Sentence2Base[BaseTokenTest with TokenWithStringLike with TokenWithPosTagLike] =>
  def toPrettyString = tokens.map (_.toPrettyString) mkString " "
}

trait SentenceWithOffsetsLike {
  this: Sentence2Base[BaseTokenTest with TokenWithOffsetsLike] =>
  def offsets = CharOffsets(tokens.head.offsets.start,tokens.last.offsets.end)
}

trait SentenceWithSyntaxAnnotationLike {
  this: Sentence2Base[BaseTokenTest] =>
  val syntax = SyntaxAnnotation
}

trait SentenceWithIEAnnotationLike {
  this: Sentence2Base[BaseTokenTest] =>
  val ie = IEAnnotation
}