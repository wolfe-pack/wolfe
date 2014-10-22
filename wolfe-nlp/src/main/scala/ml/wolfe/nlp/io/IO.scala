package ml.wolfe.nlp.io

import java.io.{PrintStream, File}

import scala.io.Source

/**
 * @author Sebastian Riedel
 */
object IO {

  def saveTSV[T](coll:Iterable[T], file:File, delim:String = "\t")(mapper:T => Seq[String]) {
    val out = new PrintStream(file)
    for (e <- coll) out.println(mapper(e).mkString(delim))
    out.close()

  }

  def loadTSV[T](file:File, delim:String = "\t", skip:Int = 0, limit:Int = Int.MaxValue)(mapper:Array[String] =>T) = {
    val source = Source.fromFile(file)
    val lines = source.getLines().drop(skip).take(limit).filter(_ != "")
    var processed = 0
    def processLine(line:String) = {
      processed += 1
      if (processed % 1000000 == 0) println(processed)
      mapper(line.split(delim))
    }
    val result = lines.map(processLine).toList
    source.close()
    result
  }

  def processTSV[T](file:File, delim:String = "\t", skip:Int = 0, limit:Int = Int.MaxValue)(body:Array[String] => Unit) = {
    val source = Source.fromFile(file)
    val lines = source.getLines().drop(skip).take(limit)
    var processed = 0
    def processLine(line:String) = {
      processed += 1
      if (processed % 1000000 == 0) println(processed)
      body(line.split(delim))
    }
    val result = lines.foreach(processLine)
    source.close()
  }


}
