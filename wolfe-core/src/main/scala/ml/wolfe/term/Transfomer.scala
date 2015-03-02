package ml.wolfe.term

/**
 * @author riedel
 */
object Transfomer {

  def depthFirst(term:Term[Dom])(partialFunction: PartialFunction[Term[Dom],Term[Dom]]): term.domain.Term = {
    val result = term match {
      case n:NAry =>
        val transformed = n.arguments map ((t:Term[Dom]) => depthFirst(t)(partialFunction).asInstanceOf[n.ArgumentType])
        val copied = n.copy(transformed)
        copied
      case t => t
    }
    val transformed = if (partialFunction.isDefinedAt(result)) partialFunction(result) else result
    term.domain.own(transformed.asInstanceOf[TypedTerm[term.domain.Value]])
  }

}
