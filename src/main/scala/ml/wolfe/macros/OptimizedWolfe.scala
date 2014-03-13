package ml.wolfe.macros

import scala.reflect.macros.Context
import scala.language.experimental.macros
import scala.collection.mutable

//import ml.wolfe.Wolfe._

/**
 * @author Sebastian Riedel
 */
object OptimizedWolfe extends WolfeAPI {

  override def argmax[T](data: Iterable[T])
                        (where: (T) => Boolean)
                        (obj: (T) => Double) = macro implArgmax[T]

  override def argmin[T](dom: Iterable[T])
                        (where: (T) => Boolean)
                        (obj: (T) => Double) = macro implArgmin[T]


  def implArgmax[T: c.WeakTypeTag](c: Context)
                                  (data: c.Expr[Iterable[T]])
                                  (where: c.Expr[T => Boolean])
                                  (obj: c.Expr[T => Double]) = {

    import c.universe._

    val helper = new MacroHelper[c.type](c)
    import helper._

    //information from the enclosing context
    val metaData = MetaStructuredGraph(data.tree, where.tree, obj.tree)
    val graphName = newTermName(c.fresh("graph"))

    //find annotation on objective
    val maxBy: Tree = getMaxByProcedure(obj.tree)

    val result = q"""
      import ml.wolfe._
      import ml.wolfe.MPGraph._

      ${metaData.classDef}
      val $graphName = new ${metaData.graphClassName}(null)
      $maxBy($graphName.${metaData.mpGraphName})
      $graphName.${metaData.structureName}.setToArgmax()
      $graphName.${metaData.structureName}.value()
    """

    c.Expr[T](result)

  }

  def implArgmin[T: c.WeakTypeTag](c: Context)
                                  (dom: c.Expr[Iterable[T]])
                                  (where: c.Expr[T => Boolean])
                                  (obj: c.Expr[T => Double]) = {

    import c.universe._

    val helper = new MacroHelper[c.type](c)
    import helper._

    //todo: check that domain dom is vector domain

    val gradientBased = MetaGradientBasedMinimizer(dom.tree, obj.tree)

    //    println(gradientBased.trainingCode)
    gradientBased.trainingCode match {
      case Some(code) => c.Expr[T](context.resetLocalAttrs(code.tree))
      case _ => reify(BruteForceWolfe.argmin(dom.splice)(where.splice)(obj.splice))
    }
    //    reify(BruteForceWolfe.argmin(dom.splice)(where.splice)(obj.splice))
  }

}




