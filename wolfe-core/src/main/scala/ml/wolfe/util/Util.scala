package ml.wolfe.util

import java.io.{PrintWriter, File, FileInputStream, InputStream}

import scala.collection.mutable.ArrayBuffer
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

  def breakpoint() = {
    var a = 0 // set breakpoint here for use in macro-generated code...
  }
  /**
   * Recursively descend directory, returning a list of files.
   */
  def files(directory: File): Seq[File] = {
    if (!directory.exists) throw new Error("File " + directory + " does not exist")
    if (directory.isFile) return List(directory)
    val result = new ArrayBuffer[File]
    for (entry: File <- directory.listFiles) {
      if (entry.isFile) result += entry
      else if (entry.isDirectory) result ++= files(entry)
    }
    result
  }

  /**
   * Are x and y approximately equal, to within eps?
   */
  def approxEqual(x: Double, y: Double, eps: Double = 1e-10) = {
    math.abs(x - y) < eps
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
  val timings = new mutable.HashMap[String, Long]()

  def time[A](name: String)(f: => A) = {
    val start = System.nanoTime
    val result = f
    val time: Long = TimeUnit.MILLISECONDS.convert(System.nanoTime - start, TimeUnit.NANOSECONDS)
    timings(name) = time
    result
  }

  def reported(name: String): Long = timings.getOrElse(name, -1)

  def getTimeString(seconds: Int): String = {
    def buildTimeString(seconds: Int, acc: String): String = {
      if (seconds < 60) acc + "%ds".format(seconds)
      else if (seconds < 3600) acc + buildTimeString(seconds % 60, "%dm ".format(seconds / 60))
      else if (seconds < 86400) acc + buildTimeString(seconds % 3600, "%dh ".format(seconds / 3600))
      else if (seconds < 604800) acc + buildTimeString(seconds % 86400, "%dd ".format(seconds / 86400))
      else if (seconds < 4233600) acc + buildTimeString(seconds % 604800, "%dw ".format(seconds / 604800))
      else "very long"
    }
    buildTimeString(seconds, "")
  }

  def getTimeString(milliseconds: Long): String = getTimeString((milliseconds / 1000).toInt)
}


/**
 * A function that turns "lifted" functions to Options into partial functions such that repeated calls
 * in isDefinedAt and apply are avoided by caching results.
 * @param f the lifted function to turn into a partial function.
 */
case class CachedPartialFunction[A, B](f: A => Option[B]) extends PartialFunction[A, B] {
  private var cacheArg   : A         = _
  private var cacheResult: Option[B] = None

  def cache(x: A) = {
    if (x != cacheArg) {
      cacheArg = x
      cacheResult = f(cacheArg)
    }
  }

  def isDefinedAt(x: A) = {
    cache(x)
    cacheResult.isDefined
  }

  def apply(x: A) = {
    cache(x)
    cacheResult.get
  }

}
