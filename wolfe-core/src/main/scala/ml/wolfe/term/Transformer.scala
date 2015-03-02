package ml.wolfe.term

/**
 * @author riedel
 */
object Transformer {

  def depthFirst(term:Term[Dom])(partialFunction: PartialFunction[Term[Dom],Term[Dom]]): Term[Dom] = {
    val result = term match {
      case n:NAry =>
        val transformed = n.arguments map ((t:Term[Dom]) => depthFirst(t)(partialFunction).asInstanceOf[n.ArgumentType])
        val copied = n.copy(transformed)
        copied
      case t => t
    }
    val transformed = if (partialFunction.isDefinedAt(result)) partialFunction(result) else result
    transformed
  }

  def removeOwned(term:Term[Dom]) = depthFirst(term) {
    case o:OwnedTerm[_] =>
      o.self
  }

  def flattenSums(term:Term[Dom]) = depthFirst(removeOwned(term)) {
    case Sum(args) => Sum(args flatMap {
      case Sum(inner) =>
        inner
      case a =>
        IndexedSeq(a)
    })
  }

}
