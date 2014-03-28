package ml.wolfe.macros

import scala.reflect.macros.Context

/**
 * @author Sebastian Riedel
 */
trait PatternRepository[C <: Context] extends SymbolRepository[C] {

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
    def unapply(tree: Tree): Option[(Tree, Tree, Tree)] = {
      tree match {
        case q"$sum[$sumType](${_})" if sum.symbol == scalaSymbols.sum && classes(sumType.symbol) => sum match {
          case q"$map[..${_}]($obj)(${_}).sum" if map.symbol == scalaSymbols.map =>
            val q"$dom.map" = map
            Some((dom, obj, sumType))
          case _ => None
        }
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

}
