package ml.wolfe.term

/**
 * @author riedel
 */
object Transformer {

  import TermImplicits._

  def depthFirst(term: Term[Dom])(partialFunction: PartialFunction[Term[Dom], Term[Dom]]): Term[Dom] = {
    val result = term match {
      case n: NAry =>
        val transformed = n.arguments map ((t: Term[Dom]) => depthFirst(t)(partialFunction).asInstanceOf[n.ArgumentType])
        val copied = n.copy(transformed)
        copied
      case t => t
    }
    val transformed = if (partialFunction.isDefinedAt(result)) partialFunction(result) else result
    transformed
  }

  def clean(term: Term[Dom]) = depthFirst(term) {
    case o: OwnedTerm[_] =>
      o.self
  }

  def flattenSums(term: Term[Dom]) = depthFirst(term) {
    case Sum(args) => Sum(args flatMap {
      case Sum(inner) =>
        inner
      case a =>
        IndexedSeq(a)
    })
  }

  def groundSums(term: Term[Dom]) = depthFirst(term) {
    case FirstOrderSum(indices, variable, body) =>
      val length = indices match {
        case i: VarSeqDom[_]#Term => i.length
        case i => VarSeqLength(indices)
      }
      val doubleTerms = for (i <- 0 until indices.domain.maxLength) yield {
        val element = indices match {
          case v: VarSeqDom[_]#Term => v(i)
          case v => new VarSeqApply[Dom, Term[VarSeqDom[Dom]], IntTerm](v, i.toConst)
        }
        val doubleTerm = depthFirst(body) {
          case t if t == variable => element
        }
        (body | variable << element).asInstanceOf[DoubleTerm]
        //doubleTerm.asInstanceOf[DoubleTerm]
      }
      val sumArgs = VarSeq(length, doubleTerms)
      sum(doubleTerms,length)
      //varSeqSum[Term[VarSeqDom[TypedDom[Double]]]](sumArgs)
  }

}

object Traversal {

  type Mem = Memoized[Dom, Term[Dom]]
  type SampleTerm = RangeDom#SampleTerm

  trait UniqueSampleTerm {
    def term:SampleTerm
  }
  case class Alone(term:SampleTerm, id:String = java.util.UUID.randomUUID.toString) extends UniqueSampleTerm
  case class InMem(term:SampleTerm, mem:Mem) extends UniqueSampleTerm

  def uniqueSampleTerms(term: Term[Dom]): List[UniqueSampleTerm] = {
    term match {
      case m: Memoized[_, _] =>
        val children = uniqueSampleTerms(m.term)
        for (c <- children) yield c match {
          case Alone(t,_) => InMem(t,m.asInstanceOf[Mem])
          case t => t
        }
      case s: SampleTerm => Alone(s) :: Nil
      case n:NAry => n.arguments.toList.flatMap(a => uniqueSampleTerms(a)).distinct
      case _ => Nil
    }
  }

  def distinctSampleCount(term:Term[Dom]) = uniqueSampleTerms(term).map(_.term.domain.domainSize).product



}