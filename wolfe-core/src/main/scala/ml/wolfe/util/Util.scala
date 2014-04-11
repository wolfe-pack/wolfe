package ml.wolfe.util

import java.io.{FileInputStream, InputStream}
import scala.io.Source
import scala.collection.mutable
import java.util.concurrent.TimeUnit

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

}

/**
 * Code for IRIS dataset.
 */
object Iris {

  implicit val classes = Seq(Label("Iris-setosa"), Label("Iris-versicolor"), Label("Iris-virginica"))

  case class Label(label: String)
  case class IrisData(sepalLength: Double, sepalWidth: Double, petalLength: Double, petalWidth: Double, irisClass: Label)

  def loadIris() = {
    val stream = Util.getStreamFromClassPathOrFile("ml/wolfe/datasets/iris/iris.data")
    val data = for (line <- Source.fromInputStream(stream).getLines().toBuffer if line.trim != "") yield {
      val Array(sl, sw, pl, pw, ic) = line.split(",")
      IrisData(sl.toDouble, sw.toDouble, pl.toDouble, pw.toDouble, Label(ic))
    }
    stream.close()
    data
  }
}

object Timer {
  val timings = new mutable.HashMap[String,Long]()

  def time[A](name:String)(f: => A) = {
    val start = System.nanoTime
    val result = f
    val time: Long = TimeUnit.MILLISECONDS.convert(System.nanoTime - start, TimeUnit.NANOSECONDS)
    timings(name) = time
    result
  }

  def reported(name:String):Long = timings.getOrElse(name, -1)

}
