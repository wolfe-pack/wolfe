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
    groupLinesList(lines,delim).reverse.map(_.reverse)
  }

  def groupLinesList(lines: Iterator[String], delim: String = ""): List[List[String]] = {
    lines.foldLeft(List(List.empty[String])) {
      (result, line) => if (line == delim) Nil :: result else (line :: result.head) :: result.tail
    }
  }

  def loadCoNLL[T](lines: Iterator[String], mapper: PartialFunction[Array[String], T]) =
    groupLinesList(lines).reverse.map(_.reverse.map(_.split("\\s+")).map(mapper))

}
