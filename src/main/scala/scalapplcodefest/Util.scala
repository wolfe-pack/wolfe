package scalapplcodefest

import java.io.{FileInputStream, InputStream}
import scalapplcodefest.value.SetValue
import scalapplcodefest.term.{State, Predicate, Var}
import cc.factorie.maths.ArrayOps
import java.util

/**
 * @author Sebastian Riedel
 */
object Util {

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
   * Set (possibly infinite in size) that requires a complex implementation,
   * and which implementation is not immediately needed.
   */
  class SetToBeImplementedLater[T] extends Set[T] {
    def contains(elem: T) = ???
    def +(elem: T) = ???
    def -(elem: T) = ???
    def iterator = ???
  }

  /**
   * Creates a set class that is not yet implemented
   * @tparam T type of set elements
   * @return the set.
   */
  def setToBeImplementedLater[T] = new SetToBeImplementedLater[T]

  /**
   * Loads a resource as stream. This returns either a resource in the classpath,
   * or in case no such named resource exists, from the file system.
   */
  def getStreamFromClassPathOrFile(name: String): InputStream = {
    val is: InputStream = getClass.getClassLoader.getResourceAsStream(name)
    if (is == null) {
      new FileInputStream(name)
    }
    else {
      is
    }
  }

  /**
   * Takes an iterator over lines and groups this according to a delimiter line.
   */
  def groupLines(lines: Iterator[String], delim:String = "") = {
    lines.foldLeft(Seq(Seq.empty[String])) {
      (result, line) => if (line == delim) result :+ Seq.empty else result.init :+ (result.last :+ line )
    }
  }

  def loadCoNLL(lines:Iterator[String], predicates: Seq[Predicate[Int, String]], length: Var[Int]) =
    groupLines(lines).map(conllToState(_,predicates, length))

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

  case class SetMap[T,R](set:Set[T],function:PartialFunction[T,R]) extends SetValue[R] {
    lazy val mapped = set.collect(function)
    def contains(elem: R) = mapped(elem)
    def iterator = mapped.iterator
  }

  case class SetFilter[T](set:Set[T],filter:T=>Boolean) extends SetValue[T] {
    def contains(elem: T) = !filter(elem) && set(elem)
    def iterator = set.view.iterator.filter(filter)
  }

  def main(args: Array[String]) {
    val test = SetUnion(List(SetUnion(List(Set(1,2))), SetUnion(List(SetUnion(List(Set(1,2)))))))
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
  def maxValue(s:A): Double = { var result = s(0); var i = 0; while (i < s.length) { if (s(i) > result) result = s(i); i += 1 }; result }
  def maxNormalize(s:A) { val norm = maxValue(s); this -= (s,norm)}
  def fill(s:A,v:Double) { util.Arrays.fill(s,v)}
}


object PatternMatchingVariancePlayground {
  trait Term[T]
  trait Fun[+A,+B]
  case class FunApp[A,B](fun:Term[Fun[A,B]],arg:Term[A]) extends Term[B]
  case class Constant[T](value:T) extends Term[T]
  case object AddOne extends Fun[Int,Int]

  def copy[T](term:Term[T]):Term[T] = term match {
    case FunApp(f@Constant(AddOne),arg) => FunApp(f,arg)
    case t => t
  }

}
