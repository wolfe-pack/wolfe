package scalapplcodefest.macros

import scala.reflect.macros.Context
import scala.language.experimental.macros
import scala.collection.mutable
import cc.factorie.optimize.Trainer

//import scalapplcodefest.Wolfe._

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
    val maxBy:Tree = getMaxByProcedure(obj.tree)

    val result = q"""
      import scalapplcodefest._
      import scalapplcodefest.MPGraph._

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


  //todo: I thought this macro was necessary to remember the fully expanded definition of a domain
  //todo: because it looks like definitions outside the macro call site aren't expanded
  //todo: however, I think they can be expanded through context.typeCheck so this may be unnecessary
  def all[A, B](mapper: A => B)(implicit dom: Iterable[A]): Iterable[B] = macro implAll[A, B]

  def implAll[A: c.WeakTypeTag, B: c.WeakTypeTag](c: Context)(mapper: c.Expr[A => B])(dom: c.Expr[Iterable[A]]) = {
    import c.universe._
    DomainExpansions.register(c.Expr(c.macroApplication), mapper, dom)
    reify(dom.splice map mapper.splice)
  }
}


//todo: Possibly unnecessary, see definition of 'all'.
object DomainExpansions {

  import scala.reflect.internal.util.SourceFile

  case class DomainExpansion(term: Context#Expr[Any], constructor: Context#Expr[Any], domain: Context#Expr[Any])

  case class Location(start: Int, source: SourceFile)

  val byExpression = new mutable.HashMap[Context#Expr[Any], DomainExpansion]()
  val byPosition   = new mutable.HashMap[Location, DomainExpansion]()

  def register(term: Context#Expr[Any], constructor: Context#Expr[Any], domain: Context#Expr[Any]) = {
    val expansion = DomainExpansion(term, constructor, domain)
    byExpression(constructor) = expansion
    byPosition(Location(term.tree.pos.startOrPoint, term.tree.pos.source)) = expansion
  }

  def findByPosition(start: Int, source: SourceFile) = byPosition.get(Location(start, source))

}


