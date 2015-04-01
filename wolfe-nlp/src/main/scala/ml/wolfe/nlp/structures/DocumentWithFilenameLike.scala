package ml.wolfe.nlp.structures

/**
 * @author Ingolf Becker
 * @date 01/04/2015
 */
trait DocumentWithFilenameLike {
  val filename : Option[String]
}
