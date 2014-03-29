package ml.wolfe.macros

import ml.wolfe.{BruteForceOperators, Wolfe, Operators}
import scala.reflect.macros.Context
import Wolfe._

/**
 * @author Sebastian Riedel
 */
object OptimizedOperators extends Operators {

  import scala.language.experimental.macros

  override def argmax[T, N: Ordering](overWhereOf: OverWhereOf[T, N]) = macro argmaxImpl[T, N]


  def argmaxImpl[T: c.WeakTypeTag, N: c.WeakTypeTag](c: Context)
                                                    (overWhereOf: c.Expr[OverWhereOf[T, N]])
                                                    (ord: c.Expr[Ordering[N]]) = {
    import c.universe._
    val helper = new ContextHelper[c.type](c) with OptimizedOperators[c.type]
    val result = helper.argmax(overWhereOf.tree)
    c.Expr[T](result)
  }

}

trait OptimizedOperators[C <: Context] extends MetaStructures[C]
                                               with MetaStructuredFactors[C]
                                               with Conditioner[C] {

  import context.universe._


  def inferenceCode(objRhs:Tree) = objRhs match {
    case q"$f(${ _ })" =>
      val t: Tree = f
      t.symbol.annotations.find(_.tpe.typeSymbol == wolfeSymbols.maxByInference) match {
        case Some(annotation) => q"${ annotation.scalaArgs.head }(_graph)"
        case None => q"ml.wolfe.MaxProduct(_graph,1)"
      }
    case _ => q"ml.wolfe.MaxProduct(_graph,1)"
  }


  def argmax(overWhereOf: Tree) = {

    val structName = newTermName(context.fresh("structure"))
    val trees = overWhereOfTrees(overWhereOf)
    val meta = metaStructure(trees.over)
    val Function(List(objArg), objRhs) = simplifyBlocks(trees.of)
    val objMatcher = meta.matcher(rootMatcher(objArg.symbol, q"$structName", meta))
    val factors = metaStructuredFactor(objRhs, meta, objMatcher)
    val inferCode = inferenceCode(objRhs)

    val structureDef = meta.classDef(newTermName("_graph"))

    val conditionCode = if (trees.where == EmptyTree) EmptyTree
    else {
      val Function(List(whereArg), whereRhs) = simplifyBlocks(trees.where)
      val whereMatcher = meta.matcher(rootMatcher(whereArg.symbol, q"$structName", meta))
      val conditioner = conditioning(whereRhs, whereMatcher)
      conditioner.code
    }


    val code = q"""
      val _graph = new ml.wolfe.MPGraph
      $structureDef
      val $structName = new ${ meta.className }
      $conditionCode
      _graph.setupNodes()
      ${ factors.classDef }
      val factors = new ${ factors.className }($structName)
      _graph.build()
      $inferCode
      $structName.setToArgmax()
      $structName.value()
    """

    code
  }


}