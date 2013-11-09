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


}
