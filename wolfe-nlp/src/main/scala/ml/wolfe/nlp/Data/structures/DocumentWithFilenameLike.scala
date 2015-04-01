package ml.wolfe.nlp.Data.structures

/**
 * @author Ingolf Becker
 * @date 01/04/2015
 */
trait DocumentWithFilenameLike {
  val filename : Option[String]
}
