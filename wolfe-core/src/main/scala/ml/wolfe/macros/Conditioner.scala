package ml.wolfe.macros

import scala.reflect.macros.Context

/**
 * @author Sebastian Riedel
 */
trait Conditioner[C <: Context] extends MetaStructures[C] {

  import context.universe._

  case class ConditioningCode(code: Tree, remainderOfCondition: Tree)

  class Matchers(matcher: Tree => Option[StructurePointer]) {

    object PairMatcher {
      def unapply(tree: Tree) = tree match {
        case q"$expr1 == $expr2" =>
          conditioningPair(expr1, expr2, matcher) match {
            case Some(code) => Some(code)
            case _ => conditioningPair(expr2, expr1, matcher) match {
              case Some(code) => Some(code)
              case _ => None
            }
          }
        case _ => None
      }
    }

    object SimpleCondition {
      def unapply(tree: Tree) = tree match {
        case q"$x == $value" => matcher(x) match {
          case Some(structure) => Some(q"${structure.structure}.observe($value)")
          case _ => None
        }
        case q"$value == $x" => matcher(x) match {
          case Some(structure) => Some(q"${structure.structure}.observe($value)")
          case _ => None
        }
        case _ => None
      }
    }

    object SeqSetLength {
      def unapply(tree: Tree) = tree match {
        case q"$x.size == $value" => matcher(x) match {
          case Some(StructurePointer(structure, meta: MetaSeqStructure)) => Some(q"$structure.setLength($value)")
          case _ => None
        }
        case q"$value == $x.size" => matcher(x) match {
          case Some(StructurePointer(structure, meta: MetaSeqStructure)) => Some(q"$structure.setLength($value)")
          case _ => None
        }
        case _ => None
      }
    }

  }


  def conditioningPair(expr1: Tree, expr2: Tree, matcher: Tree => Option[StructurePointer]): Option[Tree] = {
    (expr1, expr2) match {
      case (CaseClassCopy(select1, args1), CaseClassCopy(select2, args2)) => matcher(select1) match {
        case Some(StructurePointer(structure, meta: MetaCaseClassStructure)) =>
          val statements = ((args1 zip args2) zip meta.fields).map({
            case ((arg1, arg2), field)
              if arg1.symbol == wolfeSymbols.hide && arg2.symbol == wolfeSymbols.hide =>
              //todo: should also fire if it can be shown that both arguments evaluate to the same thing
              Some(EmptyTree)
            case ((q"${_}.$copyDefault1", q"${_}.$copyDefault2"), field)
              if copyDefault1.encoded.startsWith("copy$default$") && copyDefault2.encoded.startsWith("copy$default$") =>
              //observe!
              val code = q"$structure.${field.name}.observe($select2.${field.name})"
              Some(code)
            case ((arg1, arg2), field) =>
              conditioningPair(arg1, arg2, matcher)
          })
          if (statements.exists(_.isEmpty)) None
          else {
            val unwrapped = statements.map(s => s.get).filter(_ != EmptyTree)
            //todo: what if all statements are empty?
            if (unwrapped.size > 1) Some(Block(unwrapped.dropRight(1), unwrapped.last)) else Some(unwrapped.head)
          }
        case _ => None
      }
      case (q"$map1[..${_}]($arg1 => $value1)(${_})", q"$map2[..${_}]($arg2 => $value2)(${_})")
        if map1.symbol == scalaSymbols.map && map2.symbol == scalaSymbols.map =>
        val q"$select1.map" = map1
        val q"$select2.map" = map2
        matcher(select1) match {
          case Some(StructurePointer(structure, meta: MetaSeqStructure)) =>
            //todo: we should get the matcher by calling structure.meta.matcher
            val newArg1:Tree = q"$select1.apply(argIndex)"
            val newArg2:Tree = q"$select2.apply(argIndex)"
//            val t = iterableArgumentType(select1)
//            println(t)
//            newArg1.tpe = t
//            newArg2.tpe = t
            val newValue1 = transform(value1, {
              case i: Ident if i.symbol == arg2.symbol => newArg1
            })
            val newValue2 = transform(value2, {
              case i: Ident if i.symbol == arg2.symbol => newArg2
            })
            for (innerLoop <- conditioningPair(newValue1, newValue2, matcher)) yield {
              val code = q"""
                $structure.setLength($select2.size)
                $select2.indices.foreach((argIndex:Int) => {
                  $innerLoop
                 })
              """
              code
            }
          case _ => None
        }
//        println(select1)
//        println(select2)
//        None
      case _ =>
        None
    }
  }

  def conditioning(condition: Tree, matchStructure: Tree => Option[StructurePointer]): ConditioningCode = {
    val matchers = new Matchers(matchStructure)
    import matchers._
    val simplified = simplifyBlock(condition)
    simplified match {
      //      case q"$x == $value" => matchStructure(x) match {
      //        case Some(structure) =>
      //          ConditioningCode(q"${structure.structure}.observe($value)", EmptyTree)
      //        case _ => ConditioningCode(EmptyTree, condition)
      //      }
      case SimpleCondition(code) => ConditioningCode(code, EmptyTree)
      case PairMatcher(code) => ConditioningCode(code, EmptyTree)
      case SeqSetLength(code) => ConditioningCode(code, EmptyTree)
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
              context.error(x.pos, s"No specialized conditioning for $x")
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
    val normalized = helper.simplifyBlock(helper.unwrapSingletonBlocks(condition.tree))
    val q"($arg) => $rhs" = normalized
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
