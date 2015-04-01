package ml.wolfe.nlp.Data.structures

import ml.wolfe.nlp.CorefAnnotation

/**
 * @author Ingolf Becker
 * @date 01/04/2015
 */
trait DocumentWithCorefAnnotationLike {
  val coref: CorefAnnotation
}
