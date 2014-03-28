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
//    println(overWhereOf.tree)
//    reify[T](BruteForceOperators.argmax(overWhereOf.splice)(ord.splice))
  }

}

trait OptimizedOperators[C <: Context] extends MetaStructures[C]
                                               with MetaStructuredFactors[C]
                                               with Conditioner[C] {

  import context.universe._

  case class OverWhereOfTrees(over: Tree = EmptyTree, where: Tree = EmptyTree, of: Tree = EmptyTree)

  def overWhereOfTrees(tree: Tree): OverWhereOfTrees = tree match {
    case q"$of($obj)" if of.symbol == wolfeSymbols.of =>
      val q"$owo.of" = of
      overWhereOfTrees(owo).copy(of = obj)
    case q"$st($filter)" if st.symbol == wolfeSymbols.st =>
      val q"$owo.st" = st
      overWhereOfTrees(owo).copy(where = filter)
    case q"$where($filter)" if where.symbol == wolfeSymbols.where =>
      val q"$owo.where" = where
      overWhereOfTrees(owo).copy(where = filter)
    case q"$over($dom)" if over.symbol == wolfeSymbols.over =>
      OverWhereOfTrees(dom)
    case _ => OverWhereOfTrees()

  }

  def argmax(overWhereOf: Tree) = {

    val structName = newTermName(context.fresh("structure"))
    val trees = overWhereOfTrees(overWhereOf)
    val meta = metaStructure(trees.over)
    val Function(List(objArg),objRhs) = simplifyBlocks(trees.of)
    val objMatcher = meta.matcher(rootMatcher(objArg.symbol,q"$structName",meta))
    val factors = metaStructuredFactor(objRhs,meta,objMatcher)
    val structureDef = meta.classDef(newTermName("_graph"))

    val conditionCode = if (trees.where == EmptyTree) EmptyTree else {
      val Function(List(whereArg),whereRhs) = simplifyBlocks(trees.where)
      val whereMatcher = meta.matcher(rootMatcher(whereArg.symbol,q"$structName",meta))
      val conditioner = conditioning(whereRhs,whereMatcher)
      conditioner.code
    }

    val code = q"""
      val _graph = new ml.wolfe.MPGraph
      $structureDef
      val $structName = new ${meta.className}
      $conditionCode
      _graph.setupNodes()
      ${factors.classDef}
      val factors = new ${factors.className}($structName)
      _graph.build()
      ml.wolfe.MaxProduct(_graph,1)
      $structName.setToArgmax()
      $structName.value()
    """

    code
  }


}