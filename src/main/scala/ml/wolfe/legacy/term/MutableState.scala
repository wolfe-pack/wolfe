package ml.wolfe.legacy.term

import scala.collection.mutable
import ml.wolfe.legacy.value.{DeepEqualFun, Fun}
import org.scalautils.Good

/**
 * In a mutable state values can be assigned to variables in an incremental fashion. The mutable state
 * also supports storing values associated with ''paths'', where each path is a sequence of function applications
 * with constant arguments to a (possibly curried) function variable.
 @author Sebastian Riedel
 */
class MutableState extends State {

  import MutableState._

  private val buffers = new mutable.HashMap[Variable[Any], Buffer]()
  def get[T](variable: Variable[T]) = buffers.get(variable) match {
    case Some(buffer) => buffer.build(this).asInstanceOf[Option[T]]
    case None => None
  }
  def view[T](variable: Variable[T]) = buffers.get(variable) match {
    case Some(buffer) => buffer.view(this).asInstanceOf[Option[T]]
    case None => None
  }
  def domain = buffers.keySet.toSet
  def update[T](term: Term[T], value: T) {
    term match {
      case Path(path) => path.reverse match {
        case End(v) :: tail =>
          val builder = buffers.getOrElseUpdate(v, bufferFor(v.domain))
          builder(tail) = value
        case p => sys.error("Path needs to end with a variable step " + p)
      }
      case _ => sys.error(s"Can only update the state with paths, and $term is not a legal path")
    }
  }
}

object MutableState {

  type Path = List[PathStep]

  def allSettings(pathTerms: List[Term[Any]],
                  settings: Seq[MutableState => Unit] = Seq(s => {}),
                  state: State = State.empty): Seq[MutableState => Unit] =
    pathTerms match {
      case Nil => settings
      case path :: tail =>
        val newSettings = for (setting <- settings.view;
                               value <- path.domain.value(state)) yield (state: MutableState) => {
          setting(state)
          state(path) = value
        }
        allSettings(tail, newSettings, state)
    }

  def allStates(pathTerms: List[Term[Any]], state: State = State.empty) = {
    for (setting <- allSettings(pathTerms,state = state).view) yield {
      val modifiedState = new MutableState
      setting(modifiedState)
      modifiedState
    }
  }

  trait Buffer {
    def build(state: State): Option[Any]
    def view(state: State): Option[Any]

    def update(path: Path, value: Any)
  }

  class FunBuffer(dom: Term[Set[Any]], range: Term[Set[Any]]) extends Buffer {
    val map = new mutable.HashMap[Any, Buffer]
    var default: PartialFunction[Any, Any] = new PartialFunction[Any, Any] {
      def apply(v1: Any) = range.default.head
      def isDefinedAt(x: Any) = true
    }

    def createFun(fun: Any => Any, d: Set[Any], r: Set[Any]) = new Fun[Any, Any] with DeepEqualFun {
      def apply(v1: Any) = fun(v1)
      def funCandidateDom = d
      override def funDom = d
      def funRange = r
      def isDefinedAt(x: Any) = d(x)
      override def toString() = d.map(arg => s"$arg -> ${apply(arg)}").mkString("FunBuffer(",",",")")
    }

    def build(state: State) = {
      (dom.eval(state), range.eval(state)) match {
        case (Good(d), Good(r)) =>
          val mapping = for ((arg, buffer) <- map; value <- buffer.build(state)) yield arg -> value
          Some(createFun(v1 => mapping.getOrElse(v1, default(v1)), d, r))
        case _ => None
      }
    }

    def view(state: State) = {
      (dom.eval(state), range.eval(state)) match {
        case (Good(d), Good(r)) =>
          Some(createFun(v1 => map.get(v1).map(_.view(state)).getOrElse(v1, default(v1)), d, r))
        case _ => None
      }
    }

    def update(path: MutableState.Path, value: Any) = path match {
      case FunArgStep(arg) :: tail =>
        map.getOrElseUpdate(arg, bufferFor(range))(tail) = value
      case Nil =>
        default = value.asInstanceOf[Fun[Any, Any]]
      case _ =>
        sys.error("Bad path " + path)
    }
  }

  class ValueBuffer(dom: Term[Set[_]]) extends Buffer {
    var buffer: Any = dom.default.head
    def build(state: State) = Some(buffer)
    def view(state: State) = build(state)
    def update(path: MutableState.Path, value: Any) = path match {
      case Nil => buffer = value
      case _ => sys.error("Bad path " + path)
    }
  }


  def bufferFor(dom: Term[Set[_]]): Buffer = dom match {
    case AllFunctionsTerm(d, r) => new FunBuffer(d, r)
    case _ => new ValueBuffer(dom)
  }


  object Path {
    def unapply(term: Term[Any]): Option[Path] = term match {
      case v: Variable[_] => Some(List(End(v)))
      case f@FunApp(Path(path), Constant(arg)) => Some(FunArgStep(arg) :: path)
      case a@ArgOf(Path(path), Constant(index)) => Some(TupleArgStep(index) :: path)
      case _ => None
    }
  }

  sealed trait PathStep

  case class TupleArgStep(index: Int) extends PathStep

  case class FunArgStep(arg: Any) extends PathStep

  case class End(variable: Variable[Any]) extends PathStep

  def allPathTermsFrom(term: Term[Any]): List[Term[Any]] = term match {
    case f@UntypedFunTerm(dom, _) =>
      dom.value().toList.flatMap(v => allPathTermsFrom(FunApp(f.asInstanceOf[Term[Fun[Any, Any]]], Constant(v))))
    case t@ProductTerm(argDomains) =>
      argDomains.indices.toList.flatMap(i => allPathTermsFrom(ArgOf(t.asInstanceOf[Term[Product]], Constant(i))))
    case _ => List(term)
  }

  def allPathTermsIn(term: Term[Any]): List[Term[Any]] = term match {
    case p@Path(_) => List(p)
    case c: Composite[_] => c.componentSeq.toList.flatMap(allPathTermsIn)
    case _ => Nil
  }

  def allPathTerms(term: Term[Any]) = allPathTermsIn(term).flatMap(allPathTermsFrom)


}
