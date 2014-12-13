package ml.wolfe.nlp.io

/**
 * Created by narad on 8/1/14.
 */
object SexpReader {
  private var processed = 0
  val ldelim = "("
  val rdelim = ")"

  def read(filename: String): Iterator[String] = {
    val text = scala.io.Source.fromFile(filename).getLines().mkString(" ")
    Iterator.continually(readNext(text)).takeWhile(_ != None)
  }

  def read[T](filename: String, parseString: String => T): Iterator[T] = {
    val text = scala.io.Source.fromFile(filename).getLines().mkString(" ")
    Iterator.continually(parseString(readNext(text))).takeWhile(_ != null)
  }

  def readNext(text: String): String = {
    var count = 0;
    val start = processed
    if (start >= text.size)
      return null
    var letter = text.substring(processed, processed+1)
    while (processed < text.size) {
      processed += 1
      letter = text.substring(processed-1, processed)
      if (letter == ldelim) {
        count += 1
      }
      else if (letter == rdelim) {
        count -= 1
        if (count == 0) {
          return text.substring(start, processed).trim
        }
      }
    }
    return null
  }
}
