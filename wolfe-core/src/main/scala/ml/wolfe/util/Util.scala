package ml.wolfe.util

import java.io._

import cc.factorie.util.{FastLogging, Logger}

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

  def sig(x: Double) = 1.0 / (1.0 + math.exp(-x))
  def sq(x:Double) = x * x

}

/**
 * Code for IRIS dataset.
 */
object Iris {

  implicit val classes = Seq(Label("Iris-setosa"), Label("Iris-versicolor"), Label("Iris-virginica"))

  case class Label(label: String)
  case class IrisData(features:IrisFeatures, irisClass: Label)
  case class IrisFeatures(sepalLength: Double, sepalWidth: Double, petalLength: Double, petalWidth: Double)

  def loadIris() = {
    val stream = Util.getStreamFromClassPathOrFile("ml/wolfe/datasets/iris/iris.data")
    val data = for (line <- Source.fromInputStream(stream).getLines().toBuffer if line.trim != "") yield {
      val Array(sl, sw, pl, pw, ic) = line.split(",")
      IrisData(IrisFeatures(sl.toDouble, sw.toDouble, pl.toDouble, pw.toDouble), Label(ic))
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

  def reportedVerbose(name: String): String = getTimeString(reported(name))

  override def toString = timings.map({case (name,time) => s"$name: ${getTimeString(time)}"}).mkString("\n")
}


class ProgressBar(goal: Int, reportInterval: Int = 1, outputStream: OutputStream = System.err) {
  val printWriter = new PrintWriter(outputStream)

  //fixme: can lead to getting stuck in ~very long
  private var completed: Int = 1
  private var startTime = 0l

  def start() = {
    startTime = System.currentTimeMillis()
  }

  def apply(msg: String = "", lineBreak: Boolean = false) {
    if (completed == 0 && startTime == 0) start()
    if (completed % reportInterval == 0) {
      val percent = completed.toDouble / goal * 100
      val diffTime = System.currentTimeMillis() - startTime
      val estimatedTime = (((diffTime * (goal.toDouble / completed)) - diffTime) / 1000).toInt
      printWriter.print("\t[%6.2f".format(percent) + "%" + " %d/%d ".format(completed, goal) +
        "%8s".format("~" + getTimeString(estimatedTime)) + "]\t" + msg + "\r")
      if (lineBreak) printWriter.println()
    }
    if (goal == completed) printWriter.println()
    completed += 1
    printWriter.flush()
  }
}

/**
 * Hook into FACTORIE FastLogging that calls ProgressBar
 */
class ProgressLogger(maxIterations: Int, name:String, outputStream: => OutputStream = System.out) extends Logger(name, outputStream) {
  val logEveryN = if (Conf.hasPath("logEveryN")) Conf.getInt("logEveryN") else 1
  val progressBar = new ProgressBar(maxIterations, logEveryN, outputStream)
  progressBar.start()
  override def info(msg: => Any): Unit = progressBar(msg.toString, lineBreak = true)
}

trait ProgressLogging extends FastLogging {
  def maxIterations(): Int
  override val logger: Logger =
    Logger.loggerMap.getOrElseUpdate(this.getClass.getName + "progress", new ProgressLogger(maxIterations(), this.getClass.getName + "progress"))
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
