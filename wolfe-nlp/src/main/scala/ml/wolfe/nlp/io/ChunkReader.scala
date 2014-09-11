package ml.wolfe.nlp.io

/**
 * Created by narad on 9/11/14.
 */
class ChunkReader(filename: String, delim: String="^[ \t]*$", iencoding: String="UTF-8") extends Iterable[String] {
  private var processed = 0

  def iterator: Iterator[String] = {
    var lines = Array[String]()
    try {
      val src = scala.io.Source.fromFile(filename, iencoding)
      lines = src.getLines().toArray
      src.close()
    }
    catch {
      case e: Exception => System.err.println("Error reading file <%s> in ChunkReader.read (encoding:%s):\n%s".format(filename, iencoding, e.getStackTrace.mkString("\n")))
    }
    Iterator.continually(readNext(lines)).takeWhile(_ != null)
  }

  def readNext(lines: Array[String]): String = {
    var start = processed
    while (processed < lines.size) {
      val line = lines(processed)
      processed += 1
      if (line.matches(delim)) {
        return lines.slice(start, processed-1).mkString("\n")
      }
    }
    if (start == processed) {
      reset
      return null
    }
    else {
      return lines.slice(start, processed).mkString("\n")
    }
  }

  def reset = processed = 0
}

object ChunkReader {

  def read(filename: String): Iterator[String] = {
    val reader = new ChunkReader(filename)
    reader.iterator
  }
}