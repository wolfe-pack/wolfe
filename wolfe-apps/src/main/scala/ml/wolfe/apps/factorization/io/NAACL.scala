package ml.wolfe.apps.factorization.io

import java.io._

import ml.wolfe.apps._
import ml.wolfe.apps.factorization._
import org.sameersingh.scalaplot._
import org.sameersingh.scalaplot.gnuplot.GnuplotPlotter

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.util.Random
import scala.util.matching.Regex
import ml.wolfe.util._

/**
 * @author sameer
 * @author Sebastian Riedel
 * @author rockt
 */


object EvaluateNAACL extends App {
  val configFile = args.lift(0).getOrElse("./conf/eval.conf")
  val pathToLatestPredictions = args.lift(1).getOrElse("./data/out/" + Conf.getMostRecentOutDir + "/predict.txt") 
  new EvaluateNAACL(configFile, pathToLatestPredictions).eval()
}

class EvaluateNAACL(configFile: String, pathToLatestPredictions: String) {
  def eval(): Double = {
    val pathToLatest = pathToLatestPredictions.split("/").init.mkString("/") + "/"
    Conf.add(configFile)
    assert(!Conf.conf.entrySet().isEmpty, "Couldn't find configuration file.")

    val rankFileNamesAndLabels = Seq(pathToLatestPredictions + ":latest",
      //"./data/naacl2013/structured/test-rockt-F.txt:rockt-F",
      "./data/naacl2013/structured/test-mintz09.txt:Mintz09",
      "./data/naacl2013/structured/test-yao11.txt:Yao11",
      "./data/naacl2013/structured/test-surdeanu12.txt:Surdeanu12",
      //"./data/naacl2013/structured/test-riedel13-model-N.txt:N",
      "./data/naacl2013/structured/test-riedel13-model-F.txt:Riedel13-F",
      //"./data/naacl2013/structured/test-riedel13-model-NFE.txt:NFE",
      "./data/naacl2013/structured/test-riedel13-model-NF.txt:Riedel13-NF"
    )
    val rankFileNamesAndLabelsSplit = rankFileNamesAndLabels.map(name =>
      if (name.substring(3).contains(":")) Array(name.substring(0, name.lastIndexOf(":")), name.substring(name.lastIndexOf(":")) + 1)
      else Array(name, new File(name).getName)
    ).toSeq

    val rankFileNames = rankFileNamesAndLabelsSplit.map(_.apply(0))
    val labels = rankFileNamesAndLabelsSplit.map(_.apply(1))
    val rankFiles = rankFileNames.map(new File(_))
    val goldFile = new File(Conf.getString("eval.gold"))
    val relPatterns = Conf.getStringList("eval.targets").map(_.r).toSeq

    //    evaluate(rankFiles, goldFile, new PrintStream("out/latest/eval.txt"), relPatterns, labels)
    EvaluationTool.evaluateBinary(rankFiles, goldFile, System.out, relPatterns, labels, pathToGnuplotFile = pathToLatest)
  }
}



object EvaluationTool {
  def main(args: Array[String]) {
    Conf.add("./wolfe-apps/conf/eval.conf")
    val conf = Conf
    assert(!conf.conf.entrySet().isEmpty, "Couldn't find configuration file.")


    val rankFileNamesAndLabels = args.lift(1).getOrElse("out/" + conf.getMostRecentOutDir + "/nyt_pair.rank.txt") +: args.drop(2)
    val rankFileNamesAndLabelsSplit = rankFileNamesAndLabels.map(name =>
      if (name.contains(":")) name.split(":")
      else Array(name, new File(name).getName)
    )
    val rankFileNames = rankFileNamesAndLabelsSplit.map(_.apply(0))
    val labels = rankFileNamesAndLabelsSplit.map(_.apply(1))
    val rankFiles = rankFileNames.map(new File(_))
    val goldFile = new File(conf.getString("eval.gold"))
    val relPatterns = conf.getStringList("eval.targets").map(_.r).toSeq

    //    evaluate(rankFiles, goldFile, new PrintStream("out/latest/eval.txt"), relPatterns, labels)
    evaluateBinary(rankFiles, goldFile, System.out, relPatterns, labels)
  }


  @tailrec
  def factorial(n: Double, result: Double = 1): Double = if (n == 0) result else factorial(n - 1, result * n)

  def nOverK(n: Double, k: Double) = factorial(n) / (factorial(k) * factorial(n - k))

  def binomial(k: Double, n: Double, p: Double) = nOverK(n, k) * math.pow(p, k) * math.pow(1 - p, n - k)

  case class PerFileEvals(file: File, name: String, evals: mutable.HashMap[Regex, Eval] = new mutable.HashMap[Regex, Eval]()) {
    def averageMap() = evals.map(_._2.meanAvgPrecision).sum / evals.size

    def globalMap() = {
      val sum = evals.view.map(e => e._2.meanAvgPrecision * e._2.totalGoldTrue).sum
      val normalizer = evals.view.map(_._2.totalGoldTrue).sum
      sum / normalizer
    }

    def totalGoldTrue() = evals.view.map(_._2.totalGoldTrue).sum

    def averagePrecisionAt(recall: Double, depth: Int) = {
      evals.view.map(_._2.precisionAtRecall(recall, depth)).sum / evals.size
    }
  }

  def signtest(k: Int, n: Int) = {
    //p <= 2 * Sum (i=0 to k) {N!/(i!*(N-i)!)}/4
    //    val b = binomial(k,n,0.5)
    val sum = Range(0, k + 1).map(binomial(_, n, 0.5)).sum
    val result = 2.0 * sum
    result
    //    2.0 * b
    //    val nFact = factorial(n)
    //    val sum = Range(0,k+1).map( i => {
    //      val product = factorial(i) * factorial(n - i)
    //      nFact / product
    //    }).sum
    //    sum / 2.0
  }

  def evaluateBinary(rankFiles: Seq[File], gold: File, out: PrintStream,
                     relPatterns: Seq[Regex] = Conf.getStringList("eval.targets").toSeq.map(_.r),
                     names: Seq[String], pathToGnuplotFile: String = "eval/"): Double = {
    val poolDepth = Conf.conf.getInt("eval.pool-depth")
    val runDepth = Conf.conf.getInt("eval.run-depth")
    evaluate(rankFiles.zip(names).toSeq,
      loadAnnotations(new FileInputStream(gold)),
      out,
      relPatterns,
      l => extractBinaryFactFromLine(l),
      poolDepth,
      runDepth,
      pathToGnuplotFile)
  }

  def evaluate(rankFileNames: Seq[(File, String)],
               annotations: Map[(List[String], String), Annotation],
               out: PrintStream,
               relPatterns: Seq[Regex],
               extractFactFromLine: String => (List[String], String),
               poolDepth: Int,
               runDepth: Int,
               pathToEvaluationOutput: String = "eval/"): Double = {

    val allowedFacts = new mutable.HashMap[Regex, mutable.HashSet[(List[String], String)]]()
    println("Collecting facts from rank files")
    //println(rankFileNames.mkString("\t"))
    for ((rankFile, name) <- rankFileNames) {
      val counts = new mutable.HashMap[Regex, Int]()
      val missing = new mutable.HashSet[Regex]()
      missing ++= relPatterns
      val lines = Source.fromFile(rankFile).getLines()
      while (lines.hasNext && missing.nonEmpty) {
        val line = lines.next()
        if (line.trim != "") {
          val (tuple, predicted) = extractFactFromLine(line)
          val fact = tuple -> predicted

          for (pattern <- missing) {
            if (pattern.findFirstIn(predicted).isDefined) {
              allowedFacts.getOrElseUpdate(pattern, new mutable.HashSet[(List[String], String)]()) += fact
              counts(pattern) = counts.getOrElse(pattern, 0) + 1
              if (counts(pattern) == poolDepth) missing -= pattern
            }
          }
        }
      }
    }

    val calculatePrecisionAtKs = Set(50, 100, 200, 300, 400)
    val globalEvals = new mutable.HashMap[Regex, Eval]()

    println("Loading Annotations")
    for ((_, annotation) <- annotations) {
      for (pattern <- relPatterns) {
        if (pattern.findFirstIn(annotation.label).isDefined) {
          val facts = allowedFacts.get(pattern)
          val allowed = facts.map(_.apply(annotation.fact))
          if (allowed.getOrElse(false)) {
            val eval = globalEvals.getOrElseUpdate(pattern, new Eval(pattern))
            annotation.correct match {
              case true =>
                eval.goldTuplesTrue += annotation.tuple -> annotation.label
              case false =>
                eval.goldTuplesFalse += annotation.tuple -> annotation.label

            }
            eval.relations += annotation.label
          }
        }
      }
    }

    val perFileEvals = new ArrayBuffer[PerFileEvals]
    val details = false

    println("Loading Rank Files")
    //todo: first make sure that for each pattern and system we are using at most K
    //todo: annotations from that system

    for ((rankFile, name) <- rankFileNames) {
      val perFile = PerFileEvals(rankFile, name)
      import perFile._
      val counts = new mutable.HashMap[Regex, Int]()
      val missing = new mutable.HashSet[Regex]()
      missing ++= relPatterns
      evals ++= globalEvals.mapValues(_.copyGold)
      val lines = Source.fromFile(rankFile).getLines()
      while (lines.hasNext && missing.nonEmpty) {
        val line = lines.next()
        val (tuple, predicted) = extractFactFromLine(line)
        val fact = tuple -> predicted
        for (pattern <- relPatterns) {
          val eval = evals.getOrElseUpdate(pattern, new Eval(pattern))
          if (pattern.findFirstIn(predicted).isDefined) {
            eval.relations += predicted
            eval.totalGuess += 1
            eval.goldTuplesTrue(fact) -> eval.goldTuplesFalse(fact) match {
              case (true, _) =>
                eval.tp += 1
                eval.guessTuplesTrue += fact

              case (false, true) =>
                eval.fp += 1
              case (false, false) =>
            }
            eval.sumPrecision += eval.precision
            eval.precisions += eval.precision
            eval.recalls += eval.recall
            eval.missings += eval.missingLabels
            if (eval.goldTuplesTrue(fact)) {
              eval.avgPrecisionForFact(fact) = eval.avgPrecision
              eval.precisionForFact(fact) = eval.precision
            }
            if (calculatePrecisionAtKs(eval.totalGuess)) {
              eval.precisionAtK += ((eval.totalGuess, eval.missingLabels, eval.precision))
            }
            counts(pattern) = counts.getOrElse(pattern, 0) + 1
            if (counts(pattern) == runDepth) missing -= pattern

          }
        }
      }
      for (pattern <- relPatterns; eval <- evals.get(pattern)) {
        if (details) out.println(eval)
      }
      perFileEvals += perFile
    }

    implicit class PimpedFileWriter(out: FileWriter) {
      def println(s: String = "") {
        out.write(s)
        out.write("\n")
      }

      def print(s: String) = out.write(s)
    }

    if (Conf.hasPath("logFile") && Conf.getString("logFile") != "None") {
      val fileWriter = new FileWriter(Conf.getString("logFile"), true)
      val eval = perFileEvals.head
      val fields =
        if (Conf.hasPath("logFields")) Conf.getStringList("logFields")
        else Nil
      val fieldValues = fields.map(Conf.conf.getAnyRef)
      fileWriter.write(s"${eval.averageMap()}\t${eval.globalMap()}\t" + fieldValues.mkString("", "\t", "\n"))
      fileWriter.close()
    }

    //print overview table
    def printTextTable(out: FileWriter) {
      out.print("%-30s%-10s%-10s".format("Pattern", "Gold+", "Gold+-"))
      for ((perFile, index) <- perFileEvals.zipWithIndex) {
        out.print("| %-10s%-10s".format("MAP", "Missing"))
      }
      out.println()
      out.print("%50s".format(Range(0, 50).map(s => "-").mkString))
      for (perFile <- perFileEvals) {
        out.print("%22s".format(Range(0, 22).map(s => "-").mkString))
      }
      out.println()
      for (pattern <- relPatterns.sortBy(pattern => -perFileEvals.head.evals(pattern).totalGoldTrue)) {
        val first = perFileEvals.head
        out.print("%-30s%-10d%-10d".format(pattern.toString(), first.evals(pattern).goldTuplesTrue.size, first.evals(pattern).totalGold))
        for (perFile <- perFileEvals) {
          val eval = perFile.evals(pattern)
          out.print("| %-10.2f%-10d".format(
            eval.meanAvgPrecision,
            //          eval.precisionAtK.lift(1).map(_._3).getOrElse(-1.0)
            eval.missings.lift(math.min(poolDepth, eval.missings.size) - 1).getOrElse(-1)
          ))
        }
        out.println()
      }
      out.print("%-30s%-10d%-10d".format("Average", 0, 0))
      for (perFile <- perFileEvals) {
        out.print("| %-10.2f%-10d".format(perFile.averageMap(), -1))
      }
      out.println()
      out.print("%-30s%-10d%-10d".format("Global", 0, 0))
      for (perFile <- perFileEvals) {
        out.print("| %-10.2f%-10d".format(perFile.globalMap(), -1))
      }
      out.println()
    }

    //print latex table
    def printLatexTable(out: FileWriter) {
      def norm(label: String) = label.replaceAll("\\$", "").replaceAll("_", "\\\\_")
      val systemCount = perFileEvals.size

      out.println("\\documentclass{standalone}")
      out.println("\\begin{document}")
      out.println("\\begin{tabular}{ %s %s | %s }".format("l", "l", Seq.fill(systemCount)("c").mkString(" ")))
      out.println("  %20s & %s & %s \\\\".format("Relation", "\\#", perFileEvals.map(_.name).mkString(" & ")))
      out.println("\\hline")
      for (pattern <- relPatterns.sortBy(p => -perFileEvals.head.evals(p).totalGoldTrue)) {
        val first = perFileEvals.head
        val maps = perFileEvals.map(_.evals(pattern).meanAvgPrecision)
        val sorted = maps.sortBy(-_)
        def format(map: Double) = map match {
          case x if x >= sorted.head && (sorted.size == 1 || x <= sorted(1)) => "{\\em %6.2f}".format(map)
          case x if x >= sorted.head => "{\\bf %6.2f}".format(map)
          case _ => "%6.2f".format(map)
        }


        out.println("  %20s & %4d & %s \\\\".format(norm(pattern.toString()), first.evals(pattern).totalGoldTrue,
          maps.map(format).mkString(" & ")))
      }
      out.println("\\hline")
      out.println("  %20s & %4s & %s \\\\".format("MAP",
        "",
        perFileEvals.map(e => "%6.2f".format(e.averageMap())).mkString(" & ")))
      //      out.println("\\hline")
      out.println("  %20s & %4s & %s \\\\".format("Weighted MAP",
        "",
        perFileEvals.map(e => "%6.2f".format(e.globalMap())).mkString(" & ")))
      out.println("\\end{tabular}")
      out.print("\\end{document}")


      /*
      //rockt: this is hacky, but I don't know a better way to send this data to the grid search summary
      ResultCollector.MAP = perFileEvals.head.averageMap()
      ResultCollector.WMAP = perFileEvals.head.globalMap()

      for (eval <- perFileEvals)
        MAPCollector.add(eval.name, eval.averageMap(), eval.globalMap())
      */
    }

    println(s"Evaluation on ${perFileEvals.size} files...")

    val latexOutput = new FileWriter(pathToEvaluationOutput + "/table.tex")
    val textOutput = new FileWriter(pathToEvaluationOutput + "/table.txt")
    printLatexTable(latexOutput)
    printTextTable(textOutput)
    latexOutput.close()
    textOutput.close()

    //print pairwise comparisons
    out.println(("name" +: perFileEvals.map(_.name)).map(title => "%-13s".format(title)).mkString)
    for (i1 <- 0 until perFileEvals.size; i2 <- i1 + 1 until perFileEvals.size;
         pf1 = perFileEvals(i1)) {
      val cells = for (i2 <- i1 + 1 until perFileEvals.size; pf2 = perFileEvals(i2)) yield {
        var wins = 0
        var total = 0
        for (pattern <- relPatterns) {
          val eval1 = pf1.evals(pattern)
          val eval2 = pf2.evals(pattern)
          if (math.abs(eval1.meanAvgPrecision - eval2.meanAvgPrecision) > 0.001) {
            val win = eval1.meanAvgPrecision > eval2.meanAvgPrecision
            if (win) wins += 1
            total += 1
          }
        }
        val losses = total - wins

        val pValue = signtest(math.min(wins, losses), total)
        "%2d/%2d %5.3f".format(wins, losses, pValue)
      }
      out.println((pf1.name +: (Range(0, i1 + 1).map(i => "") ++ cells)).map("%-13s".format(_)).mkString)

    }

    /*  //print graph
      val evalDir = new File("eval")
      evalDir.mkdirs()
      for (pattern <- relPatterns) {
        val data_recallPrec = new XYData()
        val data_precAt = new XYData()

        val first = perFileEvals.head
        val x = Range(0, runDepth).map(_.toDouble)
        for (perFile <- perFileEvals) {
          val eval = perFile.evals(pattern)
          val raw = eval.precisionRecallCurve(Range(0, 50).map(_ / 50.0))
          val curve = eval.interpolate(raw)
          val curve_x = curve.map(_._1)
          val curve_y = curve.map(_._2)
          val y = eval.precisions.take(runDepth)
          val series = new MemXYSeries(curve_x, curve_y, perFile.name)
          val series_precAt = new MemXYSeries(x.take(y.length), y, perFile.name)
          data_recallPrec += series
          data_precAt += series_precAt
        }
        val chart = new XYChart("Precision at K", data_precAt)
        chart.showLegend = true
        chart.xlabel = "Facts"
        chart.ylabel = "Precision"
        val plotter = new GnuplotPlotter(chart)
        plotter.writeToPdf("eval/", pattern.toString().replaceAll("/", "_"))

        val chartRecallPrec = new XYChart("Recall/Precision", data_recallPrec)
        chartRecallPrec.showLegend = true
        chartRecallPrec.xlabel = "Recall"
        chartRecallPrec.ylabel = "Precision"
        val plotterRecallPrec = new GnuplotPlotter(chartRecallPrec)
        plotterRecallPrec.writeToPdf("eval/", pattern.toString().replaceAll("/", "_") + "-rp")

      }  */

    //print 11 point avg precision graph
    {
      val data_recallPrec = new XYData()
      val recalls = Seq(0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0)
      for (perFile <- perFileEvals) {
        val series = new MemXYSeries(recalls, recalls.map(r => perFile.averagePrecisionAt(r, runDepth)).toSeq, perFile.name)
        data_recallPrec += series
      }

      val chartRecallPrec = new XYChart("Averaged 11-point Precision/Recall", data_recallPrec)
      chartRecallPrec.showLegend = true
      chartRecallPrec.x.label = "Recall"
      chartRecallPrec.y.label = "Precision"
      chartRecallPrec.x.range_=(0, 1.0)
      chartRecallPrec.y.range_=(0, 1.0)
      chartRecallPrec.legendPosX = LegendPosX.Right
      chartRecallPrec.legendPosY = LegendPosY.Top
      chartRecallPrec.size = Some(3.0, 3.0)
      val plotterRecallPrec = new GnuplotPlotter(chartRecallPrec)
      try {
        plotterRecallPrec.pdf(pathToEvaluationOutput, "11pointPrecRecall")
      } catch {
        case e: Exception => println("Could not draw precision graph, error message " + e.getMessage)
      }
    }



    //    out.println("Missing:")
    //    for (pattern <- relPatterns) {
    //      val eval = evals(pattern)
    //      val missing = eval.goldTuplesTrue -- eval.guessTuplesTrue
    //      if (!missing.isEmpty) {
    //        out.println("--------")
    //        out.println("Regex: " + pattern)
    //        out.println(missing.mkString(","))
    //      }
    //    }

    perFileEvals.head.globalMap()
  }

  def extractUnaryFactFromLine(line: String): (List[String], String) = {
    val split = line.split("\\t")
    if (split.size == 3) {
      List(split(1)) -> split(2)
    } else {
      List(split(1)) -> split(3)
    }
  }

  def extractBinaryFactFromLine(line: String): (List[String], String) = {
    val split = line.split("\\t")
    if (split.size == 4) {
      val Array(arg1, arg2) = split(1).split("\\|")
      List(arg1, arg2) -> split(3)
    } else {
      List(split(1), split(2)) -> split(4)
    }
  }

  type Entity = String
  class Annotation(val tuple: List[Entity], val label: String, val correct: Boolean) {
    override def toString = "%s\t%s\t%s" format(if (correct) "1" else "0", label, tuple)

    def fact = tuple -> label
  }

  def loadAnnotations(in: InputStream, out: Option[PrintStream] = None) = {
    println("Reading in annotations...")
    val result = new mutable.HashMap[(List[Entity], String), Annotation]()
    for (line <- Source.fromInputStream(in).getLines()) {
      val fields = line.split("\\t")
      val correct = fields(0) == "1"
      val label = fields(1)
      val args = fields.drop(2).toSeq
      val tuple = List(args(0),args(1))
      result(tuple -> label) = new Annotation(tuple, label, correct)
      for (o <- out) o.println(line)
    }
    result.toMap
  }
}


class Eval(val pattern: Regex) {
  var totalGuess = 0
  type Entity = String

  def totalGoldTrue = goldTuplesTrue.size

  def totalGoldFalse = goldTuplesFalse.size

  var name: String = "N/A"

  def totalGold = totalGoldTrue + totalGoldFalse

  var tp = 0
  var fp = 0
  var sumPrecision = 0.0
  val precisions = new ArrayBuffer[Double]()
  val recalls = new ArrayBuffer[Double]()
  val missings = new ArrayBuffer[Int]()

  def interpolate(curve: Seq[(Double, Double)]) = {
    for (((r, p), index) <- curve.zipWithIndex) yield r -> curve.view.drop(index).map(_._2).max
  }

  def precisionRecallCurve(recallLevels: Seq[Double]) = {
    val p = 1.0 +: precisions
    val r = 0.0 +: recalls
    val result = new ArrayBuffer[(Double, Double)]
    var currentLevelIndex = 0
    def precAt(index: Int) = if (index == -1) 0.0 else p(index)
    for (level <- recallLevels) yield level -> precAt(r.indexWhere(_ >= level))
    //      var currentLevel = recallLevels(currentLevelIndex)
    //      var currentIndex = 0
    //      while (currentIndex < p.size && currentLevelIndex < recallLevels.size) {
    //        currentLevel = recallLevels(currentLevelIndex)
    //        val prec = p(currentIndex)
    //        val rec = r(currentIndex)
    //        if (rec >= currentLevel) {
    //          currentLevelIndex += 1
    //          //          result += rec -> prec
    //          result += currentLevel -> prec
    //
    //        }
    //        currentIndex += 1
    //      }
    //      result
  }

  var precisionCount = 0
  var mapDone = false
  val precisionAtK = new ArrayBuffer[(Int, Int, Double)]
  val avgPrecisionForFact = new mutable.HashMap[(List[Entity], String), Double]
  val precisionForFact = new mutable.HashMap[(List[Entity], String), Double]


  val relations = new mutable.HashSet[String]
  val goldTuplesTrue = new mutable.HashSet[(List[Entity], String)]
  val goldTuplesFalse = new mutable.HashSet[(List[Entity], String)]
  val guessTuplesTrue = new mutable.HashSet[(List[Entity], String)]

  def copyGold = {
    val result = new Eval(pattern)
    result.relations ++= relations
    result.goldTuplesTrue ++= goldTuplesTrue
    result.goldTuplesFalse ++= goldTuplesFalse
    result.guessTuplesTrue ++= guessTuplesTrue
    result
  }

  def meanAvgPrecision = {
    var result = 0.0
    for (fact <- goldTuplesTrue) {
      val avgPrec = precisionForFact.getOrElse(fact, 0.0)
      result += avgPrec
    }
    result / goldTuplesTrue.size
  }

  def precisionAtRecall(recall: Double, depth: Int) = {
    if (recall == 0.0) 1.0
    else {
      val max = math.min(depth, precisions.size)
      val filtered = Range(0, max).filter(i => recalls(i) >= recall)
      if (filtered.size == 0) 0.0 else filtered.map(precisions(_)).max
    }
  }

  def precision = tp.toDouble / totalGuess

  def recall = tp.toDouble / totalGoldTrue

  def avgPrecision = sumPrecision / totalGuess

  def missingLabels = totalGuess - tp - fp

  override def toString = {
    """------------------
      |Pattern:       %s
      |Relations:     %s
      |Total Guess:   %d
      |Total Gold(T): %d
      |Total Gold(F): %d
      |True Pos:      %d
      |Precision:     %f
      |Recall:        %f
      |Avg Prec:      %f
      |Avg Prec#:     %d
      |MAP:           %f
      |Prec. at K:    %s""".stripMargin.format(pattern.toString(), relations.mkString(","),
      totalGuess, totalGoldTrue, totalGoldFalse, tp,
      precision, recall, avgPrecision, precisionCount, meanAvgPrecision, precisionAtK.mkString(", "))
  }
}


object LoadNAACL extends App {
  def apply(k: Int = 100, subsample: Double = 1.0): TensorKB = {
    val kb = if (Conf.getBoolean("mf.use-features")) new TensorKB(k) with Features else new TensorKB(k)

    //loading cells
    val zipFile = new java.util.zip.ZipFile(new File(this.getClass.getResource("/naacl2013.txt.zip").toURI))
    import collection.JavaConverters._

    val List(entry) = zipFile.entries.asScala.toList
    val facts = Source.fromInputStream(zipFile.getInputStream(entry)).getLines()

    val rand = new Random(0l)

    for {
      fact <- facts
      Array(r, e1, e2, typ, target) = fact.split("\t")
    } {
      val cellType = typ match {
        case "Train" => CellType.Train
        case "Test" => CellType.Test
        case "Dev" => CellType.Dev
        case "Observed" => CellType.Observed
      }

      //only subsamples FB relations
      if (subsample == 1.0 || cellType != CellType.Train || !r.startsWith("REL$") || rand.nextDouble() < subsample) {
        val cell = Cell(r, (e1, e2), DefaultIx, target.toDouble, cellType)
        kb += cell
      }
    }

    //loading formulae
    if (Conf.hasPath("naacl.formulaeFile") && (Conf.getString("naacl.formulaeFile") != "None" || Conf.getString("mf.mode") == "mf")) {
      val formulaePath = Conf.getString("naacl.formulaeFile")
      println("Loading formulae form " + formulaePath)

      val lines = Source.fromFile(formulaePath).getLines()

      val start = if (Conf.hasPath("mf.formulaeStart")) Conf.getInt("mf.formulaeStart") else 0
      val end = if (Conf.hasPath("mf.formulaeEnd")) Conf.getInt("mf.formulaeEnd") else Int.MaxValue

      val formulae = lines.mkString("\n").split("\n\n")

      for {
        formulaEntry <- formulae
        Array(stats, formula) = formulaEntry.split("\n")
      } {

        val Array(numberRaw, dataScore, dataPremises, mfScore, mfPremises) =
          if (stats.split("\t").size == 5) stats.split("\t")
          else stats.split("\t").init

        val number = numberRaw.drop(2).toInt

        if (!formula.startsWith("//") && start <= number && number <= end) {
          val Array(head, tail) = formula.split(" => ")
          val body = tail.split("\t").head
          body.head match {
            case '!' => kb += ImplNeg(head, body.tail)
            case _ => kb += Impl(head, body)
          }
        }
      }
    }

    kb
  }

  val tensorKB = apply()

  println(tensorKB.numCells)
}



object WriteNAACL extends App {
  def apply(db: TensorDB, filePath: String = "./data/out/predict.txt"): Unit = {
    val writer = new FileWriter(filePath)
    val testCellsWithPrediction = db.testCells.map(c => {
      val r = c.key1
      val ep = c.key2

      //if test cell has been inferred logically take target otherwise predict using distributed representation
      val p = db.inferredCellsMap.get(r, ep).map(_.target).getOrElse(db.prob(r, ep))

      (c, p)
    }).sortBy(-_._2)


    testCellsWithPrediction.foreach { case (cell, p) =>
      val (e1, e2) = cell.key2.asInstanceOf[(String, String)]
      writer.write(s"$p\t$e1|$e2\t" + "REL$NA" + s"\t${cell.key1}\n")
    }
    writer.close()
  }
}

