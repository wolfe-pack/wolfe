package ml.wolfe.nlp.structures

import ml.wolfe.nlp.DiscourseAnnotation

/**
 * @author Ingolf Becker
 * @date 01/04/2015
 */
trait DocumentWithDiscourseAnnotation {
  val discourse: DiscourseAnnotation
}
