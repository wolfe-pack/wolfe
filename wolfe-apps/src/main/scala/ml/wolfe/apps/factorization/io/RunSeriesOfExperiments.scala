package ml.wolfe.apps.factorization.io

import java.io.{FileWriter, File}
import java.util.concurrent.Executors

import com.typesafe.config.{Config, ConfigFactory}
import ml.wolfe.util.{OverrideConfig, Conf}

import scala.collection.mutable
import scala.concurrent.ExecutionContext

/**
 * @author rockt
 */
object RunSeriesOfExperiments extends App {
  type Series = Map[String, Seq[Any]]
  type Conf = Map[String, Any]

  implicit class Crossable[X](xs: Traversable[X]) {
    def cross[Y](ys: Traversable[Y]) = for { x <- xs; y <- ys } yield Seq(x, y)
    def flatCross[Y](ys: Traversable[Y]) =
      for { x <- xs; y <- ys } yield x match {
        case ls: Seq[_] => ls ++ Seq(y)
        case _ => Seq(x, y)
      }
  }

  def apply(series: Series, threads: Int = 1): Unit = {
    val confSeq: Seq[Conf] = series.map { case (key, value) =>
      (value zip (Stream continually Seq(key)).flatten).map(_.swap)
    }.foldLeft(Seq[Any](Nil)) { case (seqs, seq) => (seqs flatCross seq).toSeq }
    .map { case seq: Seq[(String, Any)] => seq.toMap}

    this.runConfs(confSeq, threads)
  }

  def runConfs(confs: Seq[Conf], threads: Int = 1): Unit = {
    import scala.concurrent.{future, blocking, Future, Await}
    import scala.concurrent.duration._

    implicit val context = new ExecutionContext {
      val threadPool = Executors.newFixedThreadPool(threads)

      def execute(runnable: Runnable) {
        threadPool.submit(runnable)
      }

      def reportFailure(t: Throwable) {}
    }

    val configsFut: Traversable[Future[ForkRunMatrixFactorization]] = confs.map {
      c => future {
        blocking {
          val newConfPath = File.createTempFile(System.nanoTime().toString, null).getAbsolutePath
          OverrideConfig(c, newConfPath, "wolfe-apps/conf/mf.conf")
          ForkRunMatrixFactorization(newConfPath)
        }
      }
    }

    //waiting until experiments are finished; not longer than a year ;)
    val resultsFut: Future[Traversable[ForkRunMatrixFactorization]] = Future.sequence(configsFut)

    import scala.language.postfixOps
    Await.result(resultsFut, 365 days)
  }

  val series = Map(
    //"mf.subsample" -> Seq(0.001),
    "mf.maxIter" -> Seq(2),
    "formulaeFile" -> Seq("None", "data/formulae/curated-50-100.txt")
  )
  apply(series, 2)
}

case class ForkRunMatrixFactorization(pathToConf: String = "conf/mf.conf") {
  import scala.sys.process._
  println(Process(Seq(
    "sbt",
    "project wolfe-apps",
    "vmargs -Xmx4G",
    s"run-main ml.wolfe.apps.factorization.MatrixFactorization $pathToConf"
  )).!)
}