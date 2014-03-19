package ml.wolfe.macros

import scala.reflect.macros.Context

/**
 * @author Sebastian Riedel
 */
trait PatternRepository[C<:Context] extends SymbolRepository[C] {

  import context.universe._

  class InfixApply(ops:Set[Symbol]) {
    def unapply(tree:Tree):Option[(Tree,Tree)] = tree match {
      case q"$arg1Op($arg2)" if ops(arg1Op.symbol) => arg1Op match {
        case q"$arg1.${_}" => Some(arg1,arg2)
        case _ => None
      }
      case _ => None
    }
  }

  object ApplyAnd extends InfixApply(Set(scalaSymbols.and))
  object ApplyPlus extends InfixApply(scalaSymbols.doublePluses)

}
