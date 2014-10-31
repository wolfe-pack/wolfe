package ml.wolfe.apps.factorization.io

import java.io.{FileWriter, File}
import java.text.SimpleDateFormat
import java.util.Date
import java.util.concurrent.Executors

import com.typesafe.config.{Config, ConfigFactory}
import ml.wolfe.util.{OverrideConfig, Conf}
import org.joda.time.LocalDate

import scala.collection.mutable
import scala.concurrent.ExecutionContext

/**
 * @author rockt
 */
object RunSeriesOfExperiments {
  type Series = Map[String, Seq[Any]]
  type Confs = Seq[Map[String, Any]]

  implicit class Crossable[X](xs: Traversable[X]) {
    def cross[Y](ys: Traversable[Y]) = for { x <- xs; y <- ys } yield Seq(x, y)
    def flatCross[Y](ys: Traversable[Y]) =
      for { x <- xs; y <- ys } yield x match {
        case ls: Seq[_] => ls ++ Seq(y)
        case _ => Seq(x, y)
      }
  }

  def apply(series: Series, threads: Int = 1, confPath: String = "wolfe-apps/conf/mf.conf"): Unit = {
    val confSeq: Confs = series.map { case (key, value) =>
      (value zip (Stream continually Seq(key)).flatten).map(_.swap)
    }.foldLeft(Seq[Any](Nil)) { case (seqs, seq) => (seqs flatCross seq).toSeq }
                         .map { case seq: Seq[(String, Any)] => seq.toMap }

    val confSeqWithCollectFields = if (Conf.hasPath("logFile")) {
      val fields = confSeq.head.keys.toList
      //write header to collectResults
      val fileWriter = new FileWriter(Conf.getString("logFile"), true)
      fileWriter.write("\n//" + new SimpleDateFormat("dd/MM/yyyy HH:mm:ss").format(new Date()) + "\t" + confPath + "\n")
      fileWriter.write(fields.mkString("", "\t", "\t") + "MAP\twMAP\n")
      fileWriter.flush()
      fileWriter.close()

      confSeq.map(_ ++ Map(
        "logFields" -> fields
      ))
    } else confSeq

    runConfs(confSeqWithCollectFields, threads, confPath)
  }

  def runConfs(confs: Confs, threads: Int = 1, confPath: String = "wolfe-apps/conf/mf.conf"): Unit = {
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
          OverrideConfig(c, newConfPath, confPath)
          ForkRunMatrixFactorization(newConfPath)
        }
      }
    }

    //waiting until experiments are finished; not longer than a year ;)
    val resultsFut: Future[Traversable[ForkRunMatrixFactorization]] = Future.sequence(configsFut)

    import scala.language.postfixOps
    Await.result(resultsFut, 365 days)
  }
}

case class ForkRunMatrixFactorization(pathToConf: String = "conf/mf.conf") {
  import scala.sys.process._
  val userDir = System.getProperty("user.dir")

  //check whether in run from the right directory
  val correctDir = if (userDir.endsWith("/wolfe")) userDir else userDir.split("/").init.mkString("/")

  println(Process(Seq(
    "sbt",
    "project wolfe-apps",
    "vmargs -Xmx4G",
    s"run-main ml.wolfe.apps.factorization.MatrixFactorization $pathToConf"), new File(correctDir)
  ).!)
}

object SubsampleExperiments extends App {
  val threads = args.lift(0).getOrElse("1").toInt
  val confPath = args.lift(1).getOrElse("conf/mf.conf")
  val logFilePath = args.lift(2).getOrElse("data/out/experiments.log")

  Conf.add(OverrideConfig(Map("logFile" -> logFilePath), confPath + ".tmp", confPath))

  val series = Map(
    "mf.subsample" -> (0.1 to 1.0 by 0.1).toSeq,
    "formulaeFile" -> Seq("None", "data/formulae/curated-50-100.txt")
  )

  RunSeriesOfExperiments(series, threads, confPath)
  System.exit(0)
}