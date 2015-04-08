package ml.wolfe.term

import ml.wolfe.util.ObjectId

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

  def depthFirstAndReuse(term: AnyTerm, mapping: Map[ObjectId[AnyTerm], AnyTerm] = Map.empty)
                        (partialFunction: PartialFunction[AnyTerm, AnyTerm]): (AnyTerm, Map[ObjectId[AnyTerm], AnyTerm]) = {
    val termId = new ObjectId(term)
    mapping.get(termId) match {
      case Some(x) =>
        (x, mapping)
      case None =>
        val (result, newMap) = term match {
          case n: NAry =>
            val (mappings, arguments) = n.arguments.foldLeft((mapping, IndexedSeq.empty[n.ArgumentType])) {
              case ((map, args), arg) =>
                val (t, m) = depthFirstAndReuse(arg, map)(partialFunction)
                (map ++ m, args :+ t.asInstanceOf[n.ArgumentType])
            }
            val copied = n.copy(arguments)
            (copied, mappings)
          case t =>
            (t, mapping)
        }
        val transformed = if (partialFunction.isDefinedAt(result)) partialFunction(result) else result
        (transformed, newMap + (termId -> transformed))
    }
  }

  def depthLastAndReuse(term: AnyTerm, mapping: Map[ObjectId[AnyTerm], AnyTerm] = Map.empty)
                       (partialFunction: PartialFunction[AnyTerm, AnyTerm]): (AnyTerm, Map[ObjectId[AnyTerm], AnyTerm]) = {
    val termId = new ObjectId(term)
    mapping.get(termId) match {
      case Some(x) =>
        (x, mapping)
      case None =>
        if (partialFunction.isDefinedAt(term)) {
          val result = partialFunction(term)
          (result, mapping + (termId -> result))
        } else {
          term match {
            case n: NAry =>
              val (mappings, arguments) = n.arguments.foldLeft((mapping, IndexedSeq.empty[n.ArgumentType])) {
                case ((map, args), arg) =>
                  val (t, m) = depthLastAndReuse(arg, map)(partialFunction)
                  (map ++ m, args :+ t.asInstanceOf[n.ArgumentType])
              }
              val copied = n.copy(arguments)
              (copied, mappings)
            case t =>
              (t, mapping)
          }
        }
    }
  }


  def depthLast(term: AnyTerm)(partialFunction: PartialFunction[AnyTerm, AnyTerm]): AnyTerm = {
    if (partialFunction.isDefinedAt(term)) partialFunction(term)
    else term match {
      case n: NAry =>
        val transformed = n.arguments map ((t: AnyTerm) => depthLast(t)(partialFunction).asInstanceOf[n.ArgumentType])
        val copied = n.copy(transformed)
        copied
      case t =>
        t
    }
  }

  def replace[D <: Dom](term: AnyTerm)(variable: Var[D], value: Term[D]): AnyTerm = {
    depthLast(term) {
      case t if !t.vars.contains(variable) =>
        t
      case t: Memoized[_, _] =>
        Substituted(t, variable, value)
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
    case VarSeqLength(a: Atom[_]) => LengthAtom[VarSeqDom[Dom]](a.asInstanceOf[Atom[VarSeqDom[Dom]]])
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

  def precalculate(term: AnyTerm) = depthLastAndReuse(term) {
    case t if t.isStatic => Precalculated(t)
  }._1

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