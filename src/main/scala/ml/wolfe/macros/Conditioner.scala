package ml.wolfe.macros

import scala.reflect.macros.Context

/**
 * @author Sebastian Riedel
 */
trait Conditioner[C <: Context] extends MetaStructures[C] {

  import context.universe._

  case class ConditioningCode(code: Tree, remainderOfCondition: Tree)

  def conditioningPair(expr1:Tree,expr2:Tree,matcher:Tree => Option[Tree]):Option[ConditioningCode] = {
    (expr1,expr2) match {
      case (q"$select1.copy(..$arg1)",q"$select2.copy(..$arg2)") => matcher(select1) match {
        case Some(structure) =>
          //todo: assert that each field is either in both arg1 and arg2, or in neither
          //todo: for fields not in arg1 and arg2, create observe statements
          //todo: for fields in arg1 and arg2, call conditioningPair recursively
          None
        case _ => None
      }
//      case (q"$select1.map(${_} => $arg1)",q"$select2.map(${_} => $arg2)") => None
      case _ =>
        None
    }
  }

  def conditioning(condition: Tree, matchStructure: Tree => Option[Tree]): ConditioningCode = condition match {
    case q"$x == $value" => matchStructure(x) match {
      case Some(structure) => ConditioningCode(q"$structure.observe($value)", EmptyTree)
      case _ => ConditioningCode(EmptyTree, condition)
    }
    case ApplyAnd(arg1,arg2) =>
      val c1 = conditioning(arg1,matchStructure)
      val c2 = conditioning(arg2,matchStructure)
      ConditioningCode(q"{${c1.code};${c2.code}}",q"${c1.remainderOfCondition} && ${c2.remainderOfCondition}")
    case x => matchStructure(x) match {
      case Some(structure) => ConditioningCode(q"$structure.observe(true)", EmptyTree)
      case None => {
        inlineOnce(x) match {
          case Some(inlined) => conditioning(inlined, matchStructure)
          case None =>
            context.warning(x.pos, s"No specialized conditioning for $x")
            ConditioningCode(EmptyTree, condition)
        }
      }
    }
  }


}

object Conditioner {

  import scala.language.experimental.macros

  def conditioned[T](sampleSpace: Iterable[T], condition: T => Boolean) = macro conditionedImpl[T]

  def conditionedImpl[T: c.WeakTypeTag](c: Context)(sampleSpace: c.Expr[Iterable[T]],
                                                    condition: c.Expr[T => Boolean]) = {
    import c.universe._
    val graphName = newTermName("_graph")
    val structName = newTermName("structure")

    val helper = new ContextHelper[c.type](c) with MetaStructures[c.type] with Conditioner[c.type]
    println(helper.scalaSymbols.and)
    val meta = helper.metaStructure(sampleSpace.tree)
    val q"($arg) => $rhs" = condition.tree
    val root = helper.rootMatcher(arg.symbol, q"$structName.asInstanceOf[${meta.className}]")
    val matcher = meta.matcher(root)
    val conditionCode = helper.conditioning(rhs, matcher)
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
