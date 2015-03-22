package ml.wolfe.nlp

import scala.collection.generic.CanBuildFrom
import scala.language.implicitConversions

/**
* @author Ingolf Becker
*/

abstract class Document2Base[A <: BaseTokenTest, B <: Sentence2Base[A]] {
  val sentences: IndexedSeq[B]
}


trait DocumentWithStringLike[A <: BaseTokenTest with TokenWithStringLike, B <: Sentence2Base[A] with SentenceWithStringLike[A]] {
  this: Document2Base[A,B] =>
  def toPrettyString = sentences.map(_.toPrettyString) mkString " "
}

case class Document2[A <: BaseTokenTest with TokenWithStringLike,B <: Sentence2Base[A] with SentenceWithStringLike[A]]
           (sentences: IndexedSeq[B]) extends Document2Base[A,B] with DocumentWithStringLike[A,B]


object Document2 {

  def fromString[A <: BaseTokenTest with TokenWithStringLike, B <: Sentence2Base[A] with SentenceWithStringLike[A]](str: String)
           (implicit cbf: CanBuildFrom[String, B, Document2[A,B]]) : Document2[A,B] = {
    println("Document Here1")
    cbf.apply(str).result()
  }
}

trait DocumentWithIdLike {
  val id : String
}

trait DocumentWithIrAnnotationLike {
  val ir: IRAnnotation
}

trait DocumentWithCoRefAnnotationLike {
  val coref: CorefAnnotation
}

trait DocumentWithDiscourseAnnotationLike {
  val discourse: DiscourseAnnotation
}