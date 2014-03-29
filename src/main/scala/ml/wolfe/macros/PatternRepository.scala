package ml.wolfe.macros

import scala.reflect.macros.Context
import ml.wolfe.Wolfe._
import scala.Some

/**
 * @author Sebastian Riedel
 */
trait PatternRepository[C <: Context] extends SymbolRepository[C] with CodeRepository[C] {

  import context.universe._

  class InfixApply(ops: Set[Symbol]) {
    def unapply(tree: Tree): Option[(Tree, Tree)] = tree match {
      case q"$arg1Op($arg2)" if ops(arg1Op.symbol) => arg1Op match {
        case q"$arg1.${_}" => Some(arg1, arg2)
        case _ => None
      }
      case _ => None
    }
  }

  class Sum(classes: Set[Symbol]) {
    def unapply(tree: Tree): Option[(Tree, Tree, Tree, Tree)] = {
      tree match {
        case q"$sum[$sumType](${_})" if sum.symbol == scalaSymbols.sum && classes(sumType.symbol) => sum match {
          case q"$map[..${_}]($obj)(${_}).sum" if map.symbol == scalaSymbols.map =>
            val q"$dom.map" = map
            Some((dom, EmptyTree, obj, sumType))
          case _ => None
        }
        case q"$sum[$domType,$sumType]($overWhereOf)(${_})" if sum.symbol == wolfeSymbols.sum && classes(sumType.symbol)=>
          val trees = overWhereOfTrees(overWhereOf)
          Some((trees.over,trees.where,trees.of,sumType))
        case _ => None
      }
    }
  }

  class Max(classes: Set[Symbol]) {
    def unapply(tree: Tree): Option[(Tree, Tree, Tree, Tree)] = {
      tree match {
        case q"$max[$maxType](${_})" if max.symbol == scalaSymbols.max && classes(maxType.symbol) => max match {
          case q"$map[..${_}]($obj)(${_}).max" if map.symbol == scalaSymbols.map => map match {
            case q"$filter($where).map" if filter.symbol == scalaSymbols.filter =>
              val q"$dom.filter" = filter
              Some(dom, where, obj, maxType)
            case _ => None
          }
          case _ => None
        }
        case q"$max[$domType,$maxTyp]($overWhereOf)(${_})" if max.symbol == wolfeSymbols.max && classes(maxTyp.symbol) =>
          val trees = overWhereOfTrees(overWhereOf)
          Some((trees.over,trees.where,trees.of,maxTyp))
        case _ => None
      }
    }
  }

  object Dot {
    def unapply(tree: Tree): Option[(Tree, Tree)] = tree match {
      case q"ml.wolfe.Wolfe.RichVector($arg1).dot($arg2)" =>
        Some(arg1, arg2)
      case _ => None
    }
  }

  object CaseClassCopy {

    def unapply(tree: Tree): Option[(Tree, List[Tree])] = tree match {
        //todo: caseObject should be checked to be a case class, but in some contexts the tree we get is untyped
      case Block(argDefs, q"$caseObject.copy(..$args)") => //if caseObject.tpe.typeSymbol.asClass.isCaseClass =>
        val mapping = argDefs.flatMap(_.collect {case vd: ValDef => vd.symbol -> vd.rhs}).toMap
        val mappedArgs = args.map((a: Tree) => mapping.get(a.symbol))
        if (mappedArgs.exists(_.isEmpty))
          None
        else
          Some((caseObject, mappedArgs.map(_.get)))
      case q"$caseObject.copy(..$args)" if caseObject.tpe.typeSymbol.asClass.isCaseClass =>
        Some((caseObject,args))
      case _ =>
        None
    }
  }

  object ApplyAnd extends InfixApply(Set(scalaSymbols.and))
  object ApplyDoublePlus extends InfixApply(scalaSymbols.doublePluses)
  object ApplyPlus extends InfixApply(scalaSymbols.doublePluses + wolfeSymbols.vectorPlus)
  object ApplyDoubleMinus extends InfixApply(scalaSymbols.doubleMinuses)
  object DoubleSum extends Sum(Set(scalaSymbols.doubleClass))
  object Sum extends Sum(Set(scalaSymbols.doubleClass,wolfeSymbols.vectorType))
  object DoubleMax extends Max(Set(scalaSymbols.doubleClass))

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
    case _ => inlineOnce(tree) match {
      case Some(inlined) => overWhereOfTrees(tree)
      case None =>
        context.error(context.enclosingPosition, "Can't analyze over-where-of clause " + tree)
        OverWhereOfTrees()
    }

  }

}
