package ml.wolfe.macros

import scala.reflect.api.Universe

/**
 * This trait represents classes that work with respect to a specific universe. If you have generic functionality
 * for universes, best to introduce a trait that is self-typed to be InUniverse,
 * see [[ml.wolfe.macros.Transformers]] for an example.
 *
 * @author Sebastian Riedel
 */
trait InUniverse {
  type U<:Universe
  val universe:U
}