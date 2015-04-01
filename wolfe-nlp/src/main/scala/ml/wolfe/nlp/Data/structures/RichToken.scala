package ml.wolfe.nlp.Data.structures

import scala.language.implicitConversions
/**
 * @author Ingolf Becker
 * @date 01/04/2015
 */

trait RichTokenLike extends TokenLike with TokenWithPOSLike with TokenWithLemmaLike {
  def toTaggedText = toPrettyString
  implicit def stringToPOSTag(x: String) : POSTag = POSTag(x)
}

case class RichToken(word: String, offsets: CharOffsets, posTag: POSTag = null, lemma: String = null) extends RichTokenLike

