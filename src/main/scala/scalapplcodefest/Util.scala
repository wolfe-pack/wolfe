package scalapplcodefest

import java.io.{FileInputStream, InputStream}

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
    def iterator = sets.flatMap(identity).iterator
  }

  case class SetMap[T,R](set:Set[T],function:T=>R) extends SetValue[R] {
    lazy val mapped = set.map(function)
    def contains(elem: R) = mapped(elem)
    def iterator = mapped.iterator
  }

  case class SetFilter[T](set:Set[T],filter:T=>Boolean) extends SetValue[T] {
    def contains(elem: T) = !filter(elem) && set(elem)
    def iterator = set.view.iterator.filter(filter)
  }


}

