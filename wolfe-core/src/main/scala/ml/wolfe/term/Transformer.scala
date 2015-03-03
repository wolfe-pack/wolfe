package ml.wolfe.term

/**
 * @author riedel
 */
object Transformer {

  import TermImplicits._

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

  def clean(term:Term[Dom]) = depthFirst(term) {
    case o:OwnedTerm[_] =>
      o.self
  }

  def flattenSums(term:Term[Dom]) = depthFirst(term) {
    case Sum(args) => Sum(args flatMap {
      case Sum(inner) =>
        inner
      case a =>
        IndexedSeq(a)
    })
  }

  def groundSums(term:Term[Dom]) = depthFirst(term) {
    case FirstOrderSum(indices,variable,body) =>
      val length = indices match {
        case i:VarSeqDom[_]#Term => i.length
        case i => VarSeqLength(indices)
      }
      val doubleTerms = for (i <- 0 until indices.domain.maxLength) yield {
        val element = indices match {
          case v:VarSeqDom[_]#Term => v(i)
          case v => new VarSeqApply[Dom,Term[VarSeqDom[Dom]],IntTerm](v,i.toTerm)
        }
        val doubleTerm = depthFirst(body) {
          case t if t == variable => element
        }
        doubleTerm.asInstanceOf[DoubleTerm]
      }
      val sumArgs = VarSeq(length,doubleTerms)
      varSeqSum[Term[VarSeqDom[TypedDom[Double]]]](sumArgs)
  }

}
