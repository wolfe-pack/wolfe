package ml.wolfe.nlp
import collection.generic.CanBuildFrom
import scala.collection.mutable

import scala.language.implicitConversions

/**
 * @author Ingolf Becker
 */

abstract class BaseTokenTest

case class BasicTokenTest(word: String) extends BaseTokenTest with TokenWithStringLike

object BasicTokenTest {

  def newBuilder: mutable.Builder[BasicTokenTest, Sentence2[BasicTokenTest]] = {
    new mutable.ArrayBuffer[BasicTokenTest] mapResult( t=> Sentence2( t.toIndexedSeq))
  }


  implicit def basicTokenFromString(word: String) : BasicTokenTest = BasicTokenTest(word)

  implicit def canBuildFrom: CanBuildFrom[String, BasicTokenTest, Sentence2[BasicTokenTest]] = {
    new CanBuildFrom[String, BasicTokenTest, Sentence2[BasicTokenTest]] {
      override def apply(from: String): mutable.Builder[BasicTokenTest, Sentence2[BasicTokenTest]] = newBuilder
      override def apply(): mutable.Builder[BasicTokenTest, Sentence2[BasicTokenTest]] = newBuilder
    }
  }
}

trait TokenWithStringLike {
  val word: String
  def toPrettyString = word
}

trait TokenWithOffsetsLike {
  val offsets: CharOffsets
}

case class PosTag(tag: String)

object PosTag {
  implicit def stringToPosTag(tag: String) : PosTag = PosTag(tag)
}

trait TokenWithPosTagLike {
  this: BaseTokenTest with TokenWithStringLike =>
  val posTag: PosTag
  override def toPrettyString = word + "/" + posTag.tag
}

trait TokenWithLemmaLike {
  val lemma: String
}

case class FullToken(word: String, offsets: CharOffsets, posTag: PosTag, lemma: String) extends BaseTokenTest
                           with TokenWithStringLike with TokenWithOffsetsLike with TokenWithPosTagLike
                           with TokenWithLemmaLike
