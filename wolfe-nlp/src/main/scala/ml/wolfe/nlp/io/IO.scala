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

  def loadTSV[T](file:File, delim:String = "\t")(mapper:Array[String] =>T) = {
    val source = Source.fromFile(file)
    val lines = source.getLines()
    val result = lines.map(_.split(delim)).map(mapper).toList
    source.close()
    result
  }

}
