package scalapplcodefest.compiler

import scala.tools.nsc.Global

/**
 * @author sameer
 */
trait WolfeTransformer {
  def transform[T <: Global#Tree](tree: T): T
}
