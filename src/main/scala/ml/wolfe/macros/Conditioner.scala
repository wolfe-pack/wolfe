package ml.wolfe.macros

import scala.reflect.macros.Context

/**
 * @author Sebastian Riedel
 */
trait Conditioner[C <: Context] extends MetaStructures[C] {

  import context.universe._

  case class ConditioningCode(code: Tree, remainderOfCondition: Tree)

  class PairMatcher(matcher: Tree => Option[StructurePointer]) {
    def unapply(tree: Tree) = tree match {
      case q"$expr1 == $expr2" =>
        conditioningPair(expr1, expr2, matcher) match {
          case Some(code) => Some(code)
          case _ => None
        }
      case _ => None
    }
  }

  class SimpleCondition(matcher: Tree => Option[StructurePointer]) {
    def unapply(tree: Tree) = tree match {
      case q"$x == $value" => matcher(x) match {
        case Some(structure) => Some(q"${structure.structure}.observe($value)")
        case _ => None
      }
      case _ => None
    }
  }

  def conditioningPair(expr1: Tree, expr2: Tree, matcher: Tree => Option[StructurePointer]): Option[Tree] = {
    (expr1, expr2) match {
      case (CaseClassCopy(select1, args1), CaseClassCopy(select2, args2)) => matcher(select1) match {
        case Some(StructurePointer(structure,meta:MetaCaseClassStructure)) =>
          val statements = ((args1 zip args2) zip meta.fields).map({
            case ((arg1, arg2),field)
              if arg1.symbol == wolfeSymbols.hide && arg2.symbol == wolfeSymbols.hide =>
              Some(EmptyTree)
            case ((q"${_}.$copyDefault1", q"${_}.$copyDefault2"),field)
              if copyDefault1.encoded.startsWith("copy$default$") && copyDefault2.encoded.startsWith("copy$default$") =>
              //observe!
              val code = q"$structure.${field.name}.observe($select2.${field.name})"
              Some(code)
            case ((arg1,arg2),field) =>
              conditioningPair(arg1,arg2,matcher)
          })
          if (statements.exists(_.isEmpty)) None else {
            val unwrapped = statements.map(s => s.get)
            Some(Block(unwrapped.dropRight(1),unwrapped.last))
          }
        case _ => None
      }
      case (q"$select1.map($arg1 => $value1)", q"$select2.map($arg2 => $value2)") => matcher(select1) match {
        case Some(structure) =>
          //todo: check if $select1 is a Seq
          //todo: we should get the matcher by calling structure.meta.matcher
          val newMatcher = rootMatcher(arg1.symbol, q"${structure.structure}(argIndex)", structure.meta)

          val newValue2 = transform(value2, {
            case i: Ident if i.symbol == arg2.symbol => q"$select2(argIndex)"
          })
          for (innerLoop <- conditioningPair(value1, newValue2, newMatcher)) yield
            q"""
              ${structure.structure}.setSize($select2.size)
              for (argIndex <- $select2.indices) {
                $innerLoop
              }
            """
        case _ => None
      }
      case _ =>
        None
    }
  }

  def conditioning(condition: Tree, matchStructure: Tree => Option[StructurePointer]): ConditioningCode = {
    val simpleCondition = new SimpleCondition(matchStructure)
    val pairMatcher = new PairMatcher(matchStructure)
    condition match {
      //      case q"$x == $value" => matchStructure(x) match {
      //        case Some(structure) =>
      //          ConditioningCode(q"${structure.structure}.observe($value)", EmptyTree)
      //        case _ => ConditioningCode(EmptyTree, condition)
      //      }
      case simpleCondition(code) => ConditioningCode(code, EmptyTree)
      case pairMatcher(code) => ConditioningCode(code, EmptyTree)
      case ApplyAnd(arg1, arg2) =>
        val c1 = conditioning(arg1, matchStructure)
        val c2 = conditioning(arg2, matchStructure)
        ConditioningCode(q"{${c1.code};${c2.code}}", q"${c1.remainderOfCondition} && ${c2.remainderOfCondition}")
      case x => matchStructure(x) match {
        case Some(structure) => ConditioningCode(q"${structure.structure}.observe(true)", EmptyTree)
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


}

object Conditioner {

  import scala.language.experimental.macros

  def conditioned[T](sampleSpace: Iterable[T])(condition: T => Boolean) = macro conditionedImpl[T]

  def conditionedImpl[T: c.WeakTypeTag](c: Context)(sampleSpace: c.Expr[Iterable[T]])
                                       (condition: c.Expr[T => Boolean]) = {
    import c.universe._
    val graphName = newTermName("_graph")
    val structName = newTermName("structure")

    val helper = new ContextHelper[c.type](c) with MetaStructures[c.type] with Conditioner[c.type]
    println(helper.scalaSymbols.and)
    val meta = helper.metaStructure(sampleSpace.tree)
    val q"($arg) => $rhs" = condition.tree
    val root = helper.rootMatcher(arg.symbol, q"$structName.asInstanceOf[${meta.className}]", meta)
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
