package scalapplcodefest.util

import java.io.{FileInputStream, InputStream}

/**
 * @author Sebastian Riedel
 */
object Util {

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
  def groupLines(lines: Iterator[String], delim: String = ""): Seq[Seq[String]] = {
    lines.foldLeft(Seq(Seq.empty[String])) {
      (result, line) => if (line == delim) result :+ Seq.empty else result.init :+ (result.last :+ line)
    }
  }

  def loadCoNLL[T](lines: Iterator[String], mapper: PartialFunction[Array[String], T]) =
    groupLines(lines).map(_.map(_.split("\\s+")).map(mapper))

}
