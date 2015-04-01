package ml.wolfe.nlp

import breeze.linalg.SparseVector

/**
 * @author Ingolf Becker
 * @date 01/04/2015
 */
object IRAnnotation {
  val empty = IRAnnotation()
}

/**
 * Class to represent IR information for a document
 * @param docLabel an optional document label.
 * @param bowVector a vectorized bag of word representation, for example using tf-idf counts.
 */
case class IRAnnotation(docLabel:Option[String] = None,
                        bowVector:Option[SparseVector[Double]] = None)


