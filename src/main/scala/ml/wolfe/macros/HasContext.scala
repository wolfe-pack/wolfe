package ml.wolfe.macros

import scala.reflect.macros.Context

/**
 * This trait represents classes that work with respect to a specific Context. If you have generic functionality
 * for universes, best to introduce a trait that is self-typed to be HasContext,
 * see [[ml.wolfe.macros.Transformers]] for an example.
 *
 * @author Sebastian Riedel
 */
trait HasContext[C<:Context]{
  val context:C
}

/**
 * This class allows clients to mix a HasContext object w/o `{val c:c.type = c}` notation.
 * @param context the context to use.
 * @tparam C type of the context.
 */
class ContextHelper[C<:Context](val context:C) extends HasContext[C]
