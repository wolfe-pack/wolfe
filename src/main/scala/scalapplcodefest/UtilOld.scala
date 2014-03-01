package scalapplcodefest

import java.io.{FileInputStream, InputStream}
import scalapplcodefest.legacy.value.SetValue
import scalapplcodefest.legacy.term._
import cc.factorie.maths.ArrayOps
import org.scalautils.{Bad, Good, Or}
import scalapplcodefest.legacy.term.Var
import scalapplcodefest.legacy.term.Predicate

/**
 * @author Sebastian Riedel
 */
object UtilOld {

  def tooLargeToIterate = sys.error("Data structure too large to iterate over")
  def tooLargeToCount = sys.error("Data structure too large to count")
  def pointlessToAccessElement = sys.error("Pointless to access element")

  /**
   * An "representative" infinite sequence of elements. Can be used
   * for, say, the sequence of all Double objects, or String objects.
   * Such sequences are never supposed to accessed in the same way
   * actual sequences are accessed.
   */
  object InfiniteSeq extends Seq[Nothing] {
    def length = tooLargeToCount
    def iterator = tooLargeToIterate
    def apply(idx: Int) = pointlessToAccessElement
  }




  /**
   * Loads a CoNLL style file in tab separated format.
   * @param lines iterator over CoNLL style lines.
   * @param predicates predicates mapping from token index to string.
   * @param length a sentence length variable.
   * @return an iterator over state objects where each state corresponds to one sentence, and each of the
   *         given predicates is associated with a column corresponding to the predicate's order in
   *         `predicates`. The state will also have an integer variable `length` that stores the sentence
   *         length.
   *
   */
  def loadCoNLL(lines: Iterator[String], predicates: Seq[Predicate[Int, String]], length: Var[Int]) =
    util.Util.groupLines(lines).map(conllToState(_, predicates, length))

  def conllToState(lines: Seq[String], predicates: Seq[Predicate[Int, String]], length: Var[Int]) = {
    import TermDSL._
    val map = for ((line, i) <- lines.zipWithIndex;
                   (s, pred) <- line.split("\\s+") zip predicates) yield pred.atom(i) -> s
    State((map :+ length -> lines.length).toMap)
  }


}

/**
 * Classes to represent sets based on set operators compactly.
 */
object SetUtil {

  case class SetMinus[T](set: Set[T], without: Set[T]) extends Set[T] {
    def contains(elem: T) = set.contains(elem) && !without.contains(elem)
    def +(elem: T) = SetMinus(set + elem, without)
    def -(elem: T) = SetMinus(set, without + elem)
    def iterator = set.iterator.filterNot(without)
  }

  case class SetUnion[T](sets: List[Set[T]]) extends Set[T] {
    def contains(elem: T) = sets.exists(_.contains(elem))
    def +(elem: T) = SetUnion(Set(elem) :: sets)
    def -(elem: T) = SetMinus(this, Set(elem))
    def iterator = sets.flatMap(identity).toSet.iterator
  }

  case class SetMap[T, R](set: Set[T], function: PartialFunction[T, R]) extends SetValue[R] {
    lazy val mapped = set.collect(function)
    def contains(elem: R) = mapped(elem)
    def iterator = mapped.iterator
  }

  case class SetFilter[T](set: Set[T], filter: T => Boolean) extends SetValue[T] {
    def contains(elem: T) = !filter(elem) && set(elem)
    def iterator = set.view.iterator.filter(filter)
  }

  def main(args: Array[String]) {
    val test = SetUnion(List(SetUnion(List(Set(1, 2))), SetUnion(List(SetUnion(List(Set(1, 2)))))))
    val seq = test.toSeq
    println(seq)
    println(test.size)
  }

}

/**
 * Caches computation that is based on a state. Given a state, provides results from this computation. If
 * results for the same state are requested twice in row, the computation is only performed once.
 * @param doSomething the computation to do on the state.
 */
class WithStateDo(doSomething: State => Unit) {
  private var current: State = null

  /**
   * Takes the state and checks whether the last computation was on the same state (as determined by
   * object identity). If not, the `doSomething` procedure is executed on the state, otherwise no computation is done.
   * After this check the value expression is evaluated, and this expression would usually involve mutable variables
   * changed by the computation.
   * @param state the state on which the value to return depends on.
   * @param value expression that evaluates to a value.
   * @tparam T type of value.
   * @return the `value` after `doSomething` has been applied to `state`.
   */
  def get[T](state: State, value: => T) = {
    this.synchronized {
      if (!(state eq current)) {
        doSomething(state)
        current = state
      }
      value
    }
  }
}

object MoreArrayOps extends ArrayOps {
  def maxValue(s: A): Double = { var result = s(0); var i = 0; while (i < s.length) {if (s(i) > result) result = s(i); i += 1}; result }
  def maxNormalize(s: A) { val norm = maxValue(s); this -=(s, norm) }
  def fill(s: A, v: Double) { java.util.Arrays.fill(s, v) }
}


object PatternMatchingVariancePlayground {

  import scala.language.implicitConversions
  import scala.language.reflectiveCalls

  trait TypedTerm[T] extends Term {
    def typed(state: State) = eval(state).asInstanceOf[T Or Undefined]
  }

  case class TupleTerm2(arg1: Term, arg2: Term) extends Term {
    def eval(state: State) = for (a1 <- arg1.eval(state); a2 <- arg2.eval(state)) yield (a1, a2)
    def domain = ???
  }

  implicit def toRichDoubleTerm(term: TypedTerm[Double]) = new AnyRef {
    def +(that: TypedTerm[Double]) = new FunApp(Constant(DoubleAdd), TupleTerm2(term, that)) with TypedTerm[Double]
  }

  implicit def symbolToVarBuilder(symbol: Symbol) = new AnyRef {
    def of[A](domain: TypedTerm[Set[A]]) = new Var(symbol, domain) with TypedTerm[A]
  }

  def set[T](args: T*) = new Constant(args.toSet) with TypedTerm[Set[T]]

  case object DoubleAdd extends TypedFun[(Double, Double), Double] {
    def typedIsDefinedAt(x: (Double, Double)) = true
    def typedApply(x: (Double, Double)) = x._1 + x._2
    def typedFunDom = ???
    def typedFunRange = Doubles
  }

  case object Doubles extends Set[Double] {
    def contains(elem: Double) = ???
    def +(elem: Double) = ???
    def -(elem: Double) = ???
    def iterator = ???
  }

  trait Fun extends PartialFunction[Any, Any] {
    def funDom: Set[Any]
    def funRange: Set[Any]
  }

  trait TypedFun[A, B] extends Fun {
    def typedIsDefinedAt(a: A): Boolean
    def isDefinedAt(x: Any) = typedIsDefinedAt(x.asInstanceOf[A])
    def apply(v1: Any) = typedApply(v1.asInstanceOf[A])
    def typedApply(v1: A): B
    def typedFunRange: Set[B]
    def typedFunDom: Set[A]
    def funRange = typedFunRange.asInstanceOf[Set[Any]]
    def funDom = typedFunDom.asInstanceOf[Set[Any]]
  }

  trait Term {
    def eval(state: State): Any Or Undefined
    def domain: Term
  }

  case class Constant(value: Any) extends Term {
    def eval(state: State) = Good(value)
    def domain = Constant(Set(value))
  }

  case object AllFunctions extends Fun {
    def funDom = ???
    def funRange = ???
    def isDefinedAt(x: Any) = ???
    def apply(x: Any) = ???
  }

  case class Sig(variable: Var) extends Term {
    def eval(state: State) = ???
    def domain = ???
  }

  case class LambdaAbstraction(sig: Sig, body: Term) extends Term {
    def domain = FunApp(Constant(AllFunctions), TupleTerm2(sig.domain, body.domain))
    def eval(state: State) = {
      ???
    }
  }

  object FunTerm {
    def unapply(term: Term): Option[(Term, Term)] = term match {
      case Constant(f: Fun) => Some(Constant(f.funDom), Constant(f.funRange))
      case LambdaAbstraction(sig, body) => Some(sig.domain, body.domain)
      case FunApp(FunTerm(_, FunApp(Constant(AllFunctions), TupleTerm2(dom, range))), _) => Some(dom, range)
      case _ => None
    }
  }

  case class Var(name: Symbol, domain: Term) extends Term {
    def eval(state: State) = state.get(this.asInstanceOf[Variable[Any]]) match {
      case Some(value) => domain.eval(state) match {
        case Good(s: Set[_]) => if (s.asInstanceOf[Set[Any]](value)) Good(value) else Bad(???)
        case b => b
      }
      case None => Bad(VariableUndefined(???, state))
    }
  }

  case class FunApp(fun: Term, arg: Term) extends Term {
    def eval(state: State) = for (f <- fun.eval(state); arg <- arg.eval(state)) yield f.asInstanceOf[Fun](arg)
    def domain = {
      val FunTerm(_, range) = fun
      range
    }
  }

  def main(args: Array[String]) {
    val dom = set(1.0, 2.0, 3.0)
    val x = 'x of dom
    val sum = x + x
    val value = sum.typed(???).get


  }


}
