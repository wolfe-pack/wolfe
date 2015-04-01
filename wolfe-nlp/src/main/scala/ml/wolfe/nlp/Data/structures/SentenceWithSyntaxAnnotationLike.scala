package ml.wolfe.nlp.Data.structures

import ml.wolfe.nlp.SyntaxAnnotation

/**
 * @author Ingolf Becker
 * @date 31/03/2015
 */

trait SentenceWithSyntaxAnnotationLike {
  val syntax: SyntaxAnnotation
}

