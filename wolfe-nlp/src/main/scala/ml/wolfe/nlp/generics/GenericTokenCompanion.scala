package ml.wolfe.nlp.generics

import ml.wolfe.nlp.structures.{TokenLike, CharOffsets}

/**
 * @author Ingolf Becker
 * @date 13/04/2015
 */
trait GenericTokenCompanion[X <: TokenLike[X]] {
  def overwriteWord(token: X, newWord: String, newOffsets : CharOffsets): X
}
