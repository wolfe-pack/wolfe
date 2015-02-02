package ml.wolfe.macros

import scala.reflect.macros.blackbox
import scala.reflect.macros.blackbox.Context

/**
 * This trait represents classes that work with respect to a specific Context. If you have generic functionality
 * for universes, best to introduce a trait that is self-typed to be HasContext,
 * see [[ml.wolfe.macros.Transformers]] for an example.
 *
 * @author Sebastian Riedel
 */
trait HasContext[C <: blackbox.Context] {
  val context: C

  import context.universe._

  /**
   * @param trees list of trees to get distinct version of.
   * @param result result list.
   * @return distinct list of trees using `equalsStructure`.
   */
  def distinctTrees(trees: List[Tree], result: List[Tree] = Nil): List[Tree] = trees match {
    case Nil => result
    case head :: tail =>
      val distinct = if (result.exists(_.equalsStructure(head))) result else head :: result
      distinctTrees(tail, distinct)
  }

  def distinctByTrees[T](list: List[T], result: List[T] = Nil)(f:T => Tree): List[T] = list match {
    case Nil => result
    case head :: tail =>
      val distinct = if (result.map(f).exists(_.equalsStructure(f(head)))) result else head :: result
      distinctByTrees(tail, distinct)(f)
  }


}

/**
 * This class allows clients to mix a HasContext object w/o `{val c:c.type = c}` notation.
 * @param context the context to use.
 * @tparam C type of the context.
 */
class ContextHelper[C <: Context](val context: C) extends HasContext[C]
