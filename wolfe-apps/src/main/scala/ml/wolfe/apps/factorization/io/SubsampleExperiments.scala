package ml.wolfe.apps.factorization.io

import java.io.{FileWriter, File}
import java.text.SimpleDateFormat
import java.util.Date
import java.util.concurrent.Executors

import com.typesafe.config.{Config, ConfigFactory}
import ml.wolfe.util.{RunExperimentSeries, OverrideConfig, Conf}
import org.joda.time.LocalDate

import scala.collection.mutable
import scala.concurrent.ExecutionContext

/**
 * @author rockt
 */
object SubsampleExperiments extends App {
  val threads = args.lift(0).getOrElse("1").toInt
  val confPath = args.lift(1).getOrElse("conf/mf.conf")
  val logFilePath = args.lift(2).getOrElse("data/out/experiments.log")

  Conf.add(OverrideConfig(Map("logFile" -> logFilePath), confPath + ".tmp", confPath))

  val series = Map(
    "mf.subsample" -> (1 to 10).map(_ / 10.0).toSeq,
    "formulaeFile" -> Seq("None", "data/formulae/curated-50-100.txt")
  )

  RunExperimentSeries(series, threads, confPath) { conf =>
    import scala.sys.process._
    val userDir = System.getProperty("user.dir")

    //check whether in the right directory
    val correctDir = if (userDir.endsWith("/wolfe")) userDir else userDir.split("/").init.mkString("/")

    println(Process(Seq(
      "sbt",
      "project wolfe-apps",
      "vmargs -Xmx8G",
      s"run-main ml.wolfe.apps.factorization.MatrixFactorization $conf"), new File(correctDir)
    ).!)
  }

  System.exit(0)
}



