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

  object ApplyAnd extends InfixApply(Set(scalaSymbols.and))
  object ApplyPlus extends InfixApply(scalaSymbols.doublePluses)
  object ApplyMinus extends InfixApply(scalaSymbols.doubleMinuses)
  object DoubleSum extends Sum(Set(scalaSymbols.doubleClass))
  object DoubleMax extends Max(Set(scalaSymbols.doubleClass))

}
