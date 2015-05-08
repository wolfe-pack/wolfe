package ml.wolfe.nlp.io

import java.io.{InputStreamReader, BufferedReader, FileInputStream}
import java.util.zip.GZIPInputStream

/**
 * Created by narad on 9/21/14.
 */
class GZipReader(filename: String) extends Iterator[String] {
  val gzip = new GZIPInputStream(new FileInputStream(filename))
  val br = new BufferedReader(new InputStreamReader(gzip))

  var nextUp = br.readLine()

  def close() = br.close()

  def hasNext = nextUp != null

  def next() = {
    val cur = nextUp
    nextUp = br.readLine()
    cur
  }

}
