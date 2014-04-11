package ml.wolfe.macros

import scala.reflect.macros.Context
import ml.wolfe.{HierarchicalIndex, Wolfe}
import scala.language.experimental.macros

/**
 * Takes an objective with respect to some weight vector, and calculates an outer bound on all possible keys that
 * could appear with non-zero values in the gradient of the function.
 * @author Sebastian Riedel
 */
trait IndexBuilderGenerator[C <: Context] extends CodeRepository[C] {

  import context.universe._

  def generateBuilder(inputObjective:Tree):Tree = {

    ???
  }

}

object IndexBuilderGenerator {
  def generateBuilder[T](f: T => Wolfe.Vector => Double): T => HierarchicalIndex = macro generateBuilderImpl[T]

  def generateBuilderImpl[T: c.WeakTypeTag](c: Context)(f: c.Expr[T => Wolfe.Vector => Double]) = {
    val helper = new ContextHelper[c.type](c) with IndexBuilderGenerator[c.type]
    val result = helper.generateBuilder(f.tree)
    c.Expr[T => HierarchicalIndex](result)
  }


}