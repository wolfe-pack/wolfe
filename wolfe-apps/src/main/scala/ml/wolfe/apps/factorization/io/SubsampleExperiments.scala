package ml.wolfe.apps.factorization.io

import java.io.{BufferedWriter, FileWriter, File}
import java.text.SimpleDateFormat
import java.util.Date
import java.util.concurrent.Executors

import com.typesafe.config.{Config, ConfigFactory}
import ml.wolfe.util.{ProgressBar, RunExperimentSeries, OverrideConfig, Conf}
import org.joda.time.LocalDate

import scala.collection.mutable
import scala.concurrent.ExecutionContext
import scala.util.Random

/**
 * @author rockt
 */
object SubsampleExperiments extends App {
  val threads = args.lift(0).getOrElse("1").toInt
  // The file containing the formulas
  val formulaeFile = args.lift(1).getOrElse("data/formulae/curated.txt")
  // The file containing the main configuration
  val confPath = args.lift(2).getOrElse("conf/mf.conf")
  // The log files
  val logFilePath = args.lift(3).getOrElse("data/out/experiments.log")
  val runLogFilePath = args.lift(4).getOrElse("data/out/run.log")
  val runLogFile = new File(runLogFilePath)
  runLogFile.createNewFile()

  //
  Conf.add(OverrideConfig(Map("logFile" -> logFilePath), confPath + ".tmp", confPath))

  val rand = new Random(0l)

  // This generates a list of 0.1, 0.2... 1, and shuffles it (for some not so obvious reason)
  val series = Map(
    "mf.subsample" -> (1 to 10).map(_ / 10.0).toSeq,
    "formulaeFile" -> Seq("None", formulaeFile)
  ).mapValues(rand.shuffle(_))


  val progressBar = new ProgressBar(series.values.map(_.size).foldLeft(1)(_ * _), 1)
  progressBar.start()

  // RunExp... generates function series1 that takes a function with a stringToAny map as input
  val series1: ((String) => Any) => Unit = RunExperimentSeries(series, threads, confPath)

  // Series1 generates the configs which will then be executed
  series1 { conf =>
    import scala.sys.process._
    val userDir = System.getProperty("user.dir")

    //check whether in the right directory
    val correctDir = if (userDir.endsWith("/wolfe")) userDir else userDir.split("/").init.mkString("/")

    (Process(Seq(
      "sbt",
      "project wolfe-apps",
      "vmargs -Xmx8G",
      s"run-main ml.wolfe.apps.factorization.MatrixFactorization $conf"), new File(correctDir)
    ) #>> runLogFile).!!

    progressBar(conf)
  }

  System.exit(0)
}



