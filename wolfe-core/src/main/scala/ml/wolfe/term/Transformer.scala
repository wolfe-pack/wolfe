package ml.wolfe.term

/**
 * @author riedel
 */
object Transformer {

  import TermImplicits._

  def depthFirst(term: AnyTerm)(partialFunction: PartialFunction[AnyTerm, AnyTerm]): AnyTerm = {
    val result = term match {
      case n: NAry =>
        val transformed = n.arguments map ((t: AnyTerm) => depthFirst(t)(partialFunction).asInstanceOf[n.ArgumentType])
        val copied = n.copy(transformed)
        copied
      case t => t
    }
    val transformed = if (partialFunction.isDefinedAt(result)) partialFunction(result) else result
    transformed
  }

  def depthLast(term: AnyTerm)(partialFunction: PartialFunction[AnyTerm, AnyTerm]): AnyTerm = {
    if(partialFunction.isDefinedAt(term)) partialFunction(term)
    else term match {
      case n: NAry =>
        val transformed = n.arguments map ((t: AnyTerm) => depthLast(t)(partialFunction).asInstanceOf[n.ArgumentType])
        val copied = n.copy(transformed)
        copied
      case t =>
        t
    }
  }

  def replace[D <: Dom](term: AnyTerm)(variable: Var[D], value:Term[D]): AnyTerm = {
    depthLast(term){
      case t if ! t.vars.contains(variable) =>
        t
      case t: Memoized[_, _] =>
        Conditioned(t, variable, value)
      case `variable` =>
        value
    }
  }

  def clean(term: AnyTerm) = depthFirst(term) {
    case o: OwnedTerm[_] => o.self
  }

  def flattenSums(term: AnyTerm) = depthFirst(term) {
    case Sum(args) => Sum(args flatMap {
      case Sum(inner) =>
        inner
      case a =>
        IndexedSeq(a)
    })
  }

  def groundVariables(toGround: Seq[AnyVar])(term: AnyTerm) = depthFirst(term) {
    case v: Var[_] if toGround.contains(v) => VarAtom(v)
    case VarSeqApply(a: Atom[_], i) => SeqAtom[Dom, VarSeqDom[Dom]](a.asInstanceOf[Atom[VarSeqDom[Dom]]], i)
    case VarSeqLength(a :Atom[_]) => LengthAtom[VarSeqDom[Dom]](a.asInstanceOf[Atom[VarSeqDom[Dom]]])
  }

  def groundSums(term: AnyTerm) = depthFirst(term) {
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
        //(body | variable << element).asInstanceOf[DoubleTerm]
        replace(body)(variable, element).asInstanceOf[DoubleTerm]
        //doubleTerm.asInstanceOf[DoubleTerm]
      }
      val sumArgs = VarSeq(length, doubleTerms)
      sum(doubleTerms, length)
    //varSeqSum[Term[VarSeqDom[TypedDom[Double]]]](sumArgs)
  }

}

object Traversal {


  trait UniqueSampleTerm {
    def term: SampleTerm
  }

  case class Alone(term: SampleTerm, id: String = java.util.UUID.randomUUID.toString) extends UniqueSampleTerm

  case class InMem(term: SampleTerm, mem: Mem) extends UniqueSampleTerm

  def uniqueSampleTerms(term: AnyTerm): List[UniqueSampleTerm] = {
    term match {
      case m: Memoized[_, _] =>
        val children = uniqueSampleTerms(m.term)
        for (c <- children) yield c match {
          case Alone(t, _) => InMem(t, m.asInstanceOf[Mem])
          case t => t
        }
      case s: SampleTerm => Alone(s) :: Nil
      case n: NAry => n.arguments.toList.flatMap(a => uniqueSampleTerms(a)).distinct
      case _ => Nil
    }
  }

  def distinctSampleCount(term: AnyTerm) = uniqueSampleTerms(term).map(_.term.domain.domainSize).product


}