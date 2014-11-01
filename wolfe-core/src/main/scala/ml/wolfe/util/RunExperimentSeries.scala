package ml.wolfe.util

import java.io.{File, FileWriter}
import java.text.SimpleDateFormat
import java.util.Date
import java.util.concurrent.Executors

import scala.concurrent.ExecutionContext

/**
 * @author rockt
 */
object RunExperimentSeries {
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

  def apply(series: Series, threads: Int = 1, confPath: String = "wolfe-apps/conf/mf.conf")(body: String => Any): Unit = {
    val confSeq: Confs = series.map { case (key, value) =>
      (value zip (Stream continually Seq(key)).flatten).map(_.swap)
    }.foldLeft(Seq[Any](Nil)) { case (seqs, seq) => (seqs flatCross seq).toSeq }
                         .map { case seq: Seq[(String, Any)] => seq.toMap }

    val confSeqWithCollectFields = if (Conf.hasPath("logFile")) {
      val fields = confSeq.head.keys.toList
      //write header to collectResults
      val fileWriter = new FileWriter(Conf.getString("logFile"), true)
      fileWriter.write("\n//" + new SimpleDateFormat("dd/MM/yyyy HH:mm:ss").format(new Date()) + "\t" + confPath + "\n")
      fileWriter.write("MAP\twMAP\t" + fields.mkString("", "\t", "\n"))
      fileWriter.close()

      confSeq.map(_ ++ Map(
        "logFields" -> fields
      ))
    } else confSeq

    runConfs(confSeqWithCollectFields, threads, confPath)(body)
  }

  private def runConfs(confs: Confs, threads: Int = 1, confPath: String = "wolfe-apps/conf/mf.conf")(body: String => Any): Unit = {
    import scala.concurrent.{future, blocking, Future, Await}
    import scala.concurrent.duration._

    implicit val context = new ExecutionContext {
      val threadPool = Executors.newFixedThreadPool(threads)

      def execute(runnable: Runnable) {
        threadPool.submit(runnable)
      }

      def reportFailure(t: Throwable) {}
    }

    val configsFut: Traversable[Future[Any]] = confs.map {
      c => future {
        blocking {
          val newConfPath = File.createTempFile(System.nanoTime().toString, null).getAbsolutePath
          OverrideConfig(c, newConfPath, confPath)
          body(newConfPath)
        }
      }
    }

    //waiting until experiments are finished; not longer than a year ;)
    val resultsFut: Future[Traversable[Any]] = Future.sequence(configsFut)
    import scala.language.postfixOps
    Await.result(resultsFut, 365 days)
  }
}