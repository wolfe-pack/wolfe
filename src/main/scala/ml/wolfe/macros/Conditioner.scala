package ml.wolfe.macros

import scala.reflect.macros.Context

/**
 * @author Sebastian Riedel
 */
trait Conditioner[C <: Context] extends HasContext[C] with MetaStructures[C] with WolfeSymbols[C] {

  import context.universe._

  case class ConditioningCode(code: Tree, remainderOfCondition: Tree)

  def conditioning(condition: Tree, matchStructure: Tree => Option[Tree]): ConditioningCode = condition match {
    case q"$x == $value" => matchStructure(x) match {
      case Some(structure) => ConditioningCode(q"$structure.observe($value)", EmptyTree)
      case _ => ConditioningCode(EmptyTree, condition)
    }
    case x => matchStructure(x) match {
      case Some(structure) => ConditioningCode(q"$structure.observe(true)", EmptyTree)
      case _ => {
        context.warning(x.pos, s"No specialized conditioning for $x")
        ConditioningCode(EmptyTree, condition)
      }
    }
  }

  /*
    def observationSetup(metadata: MetaStructuredGraph, pred: Tree, matchStructure: Tree => Option[Tree]): ObservationSetup = pred match {
    case q"$x == $value" => matchStructure(x) match {
      case Some(structure) => ObservationSetup(q"$structure.observe($value)", EmptyTree)
      case _ => ObservationSetup(EmptyTree, pred)
    }
    case x => matchStructure(x) match {
      case Some(structure) =>
        ObservationSetup(q"$structure.observe(true)", EmptyTree)
      case _ =>
        ObservationSetup(EmptyTree, pred)
    }
  }

   */

}

object Conditioner {

  import scala.language.experimental.macros

  def conditioned[T](sampleSpace:Iterable[T],condition:T => Boolean) = macro conditionedImpl[T]

  def conditionedImpl[T : c.WeakTypeTag](c:Context)(sampleSpace:c.Expr[Iterable[T]],
                                                    condition:c.Expr[T => Boolean]) = {
    import c.universe._
    val graphName = newTermName("_graph")
    val structName = newTermName("structure")

    val helper = new ContextHelper[c.type](c) with MetaStructures[c.type] with Conditioner[c.type]
    val meta = helper.metaStructure(sampleSpace.tree)
    val q"($arg) => $rhs" = condition.tree
    val root = helper.rootMatcher(arg.symbol, q"$structName.asInstanceOf[${meta.className}]")
    val matcher = meta.matcher(root)
    val conditionCode = helper.conditioning(rhs,matcher)
    val cls = meta.classDef(graphName)
    val code = q"""
      val $graphName = new ml.wolfe.MPGraph
      $cls
      val $structName = new ${meta.className}
      ${conditionCode.code}
      $graphName.setupNodes()
      $structName
    """
    c.Expr[Structure[T]](code)
  }

}
