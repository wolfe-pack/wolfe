package ml.wolfe.macros

import scala.reflect.macros.Context

/**
 * @author Sebastian Riedel
 */
trait PatternRepository[C<:Context] extends SymbolRepository[C] {

  import context.universe._

  class InfixApply(op:Symbol) {
    def unapply(tree:Tree):Option[(Tree,Tree)] = tree match {
      case q"$arg1Op($arg2)" if arg1Op.symbol == op => arg1Op match {
        case q"$arg1.${_}" => Some(arg1,arg2)
        case _ => None
      }
      case _ => None
    }
  }

  object AndApply extends InfixApply(scalaSymbols.and)

}
