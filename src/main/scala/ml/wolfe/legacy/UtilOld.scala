package ml.wolfe.legacy

import java.io.{FileInputStream, InputStream}
import ml.wolfe.legacy.value.SetValue
import ml.wolfe.legacy.term._
import cc.factorie.maths.ArrayOps
import org.scalautils.{Bad, Good, Or}
import ml.wolfe.legacy.term.Var
import ml.wolfe.legacy.term.Predicate

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
    ml.wolfe.util.Util.groupLines(lines).map(conllToState(_, predicates, length))

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

