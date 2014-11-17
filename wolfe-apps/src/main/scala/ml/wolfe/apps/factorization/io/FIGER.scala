package ml.wolfe.apps.factorization.io

import java.io._

import ml.wolfe.apps.factorization.CellType.CellType
import ml.wolfe.apps.factorization._
import org.sameersingh.scalaplot.gnuplot.GnuplotPlotter
import org.sameersingh.scalaplot.{XYChart, XYData, MemXYSeries}
import scala.collection.mutable
import scala.io.Source
import scala.util.Random
import ml.wolfe.util._

/**
 * @author sameer
 * @author Sebastian Riedel
 * @author rockt
 */

object EvaluateFIGER {
  val STRICT      = "Strict"
  val LOOSE_MICRO = "Loose Micro"
  val LOOSE_MACRO = "Loose Macro"

  abstract class Metric(precNum: Double, precDenom: Double, recallNum: Double, recallDenom: Double) {
    def precision: Double = precNum / precDenom

    def recall: Double = recallNum / recallDenom

    def f1: Double = {
      val r: Double = recall
      val p: Double = precision
      if (p + r == 0.0) 0.0
      else (2 * p * r) / (p + r)
    }

    override def toString: String = precision + "\t" + recall + "\t" + f1
  }

  class Strict(matches: Int, P: Int, T: Int) extends Metric(matches.toDouble, P.toDouble, matches.toDouble, T.toDouble)

  class LooseMacro(sumRatioCorrectPred: Double, sumRatioCorrectTrue: Double, P: Int, T: Int)
    extends Metric(sumRatioCorrectPred, P.toDouble, sumRatioCorrectPred, T.toDouble)

  class LooseMicro(sumPred: Double, sumTrue: Double, sumCorrect: Double)
    extends Metric(sumCorrect, sumPred, sumCorrect, sumTrue)

  type Entity = List[String]

  def evaluateSingle(entities: Set[Entity],
                     annotations: Map[Entity, Set[String]],
                     predictions: Map[Entity, Set[String]]): Map[String, Metric] = {
    var exactMatch = 0
    var sumCorrectRatioPred = 0.0
    var sumCorrectRatioTrue = 0.0
    var sumCorrect = 0.0
    var sumTrue = 0.0
    var sumPred = 0.0
    var P = 0
    var T = 0
    for (e <- entities) {
      P += 1
      T += 1
      val te: Set[String] = annotations.getOrElse(e, {
        T -= 1;
        Set.empty
      })
      val pe: Set[String] = predictions.getOrElse(e, {
        P -= 1;
        Set.empty
      })
      sumPred += pe.size
      sumTrue += te.size
      if (te == pe) exactMatch += 1
      val correct = (te & pe).size.toDouble
      sumCorrect += correct
      if (pe.size > 0) sumCorrectRatioPred += (correct / pe.size)
      if (te.size > 0) sumCorrectRatioTrue += (correct / te.size)
    }
    Map(STRICT -> new Strict(exactMatch, P, T),
      LOOSE_MACRO -> new LooseMacro(sumCorrectRatioPred, sumCorrectRatioTrue, P, T),
      LOOSE_MICRO -> new LooseMicro(sumPred, sumTrue, sumCorrect))
  }

  def evaluate(predFiles: Seq[(File, String)],
               annotations: Set[(List[String], String)],
               outputDir: String, numPoints: Int = 10) {
    val entities = annotations.map(_._1).toSet
    val methodNames = predFiles.map(_._2)
    println("Num entities: " + entities.size)
    val annotationMap = annotations.groupBy(_._1).map(p => p._1 -> p._2.map(_._2).toSet).toMap
    val predictions: Seq[(String, Seq[(List[String], String, Double)])] =
      predFiles.map(fs =>
        fs._2 -> (for (line <- Source.fromFile(fs._1).getLines()) yield {
          val split = line.split("\\t")
          val score = split.head.toDouble
          val entity = List(split(1))
          val relation = split.last
          (entity, relation, score)
        }).toSeq)
    // start computing
    val results =
      (1 to numPoints).map(i => {
        val truncatedPreds = predictions.map(np => np._1 -> np._2.take(np._2.size * i / numPoints).map(p => p._1 -> p._2))
        val predMaps = truncatedPreds.map(np => np._1 -> np._2.groupBy(_._1).map(p => p._1 -> p._2.map(_._2).toSet).toMap)
        predMaps.map(np => np._1 -> {
          evaluateSingle(entities, annotationMap, np._2)
        })
      })
    // start plotting
    for (pt <- Seq(STRICT, LOOSE_MACRO, LOOSE_MICRO)) {
      println("--- %s ----" format (pt))
      val metrics = results.map(p => p.map(nm => nm._1 -> nm._2(pt)).toMap)
      val methodMetrics = methodNames.map(n => n -> metrics.map(nm => nm(n)))
      // print best F1
      methodMetrics.foreach(nm => println(nm._1 + "\t" + nm._2.maxBy(_.f1)))
      // Plotting
      // pr curve
      val prSeries = methodMetrics.map(nms => {
        val xys = nms._2.map(m => m.recall -> m.precision)
        new MemXYSeries(xys.map(_._1), xys.map(_._2), nms._1)
      })
      val prData = new XYData(prSeries: _*)
      val prChart = new XYChart("Precision Recall Curve (%s)" format (pt), prData)
      prChart.x.label = "Recall"
      prChart.y.label = "Precision"
      prChart.showLegend = true
      GnuplotPlotter.pdf(prChart, outputDir, "pr_curve" + pt.toLowerCase.replaceAll("\\s+", "_"))
      // f1 curve
      val f1Series = methodMetrics.map(nms => {
        val xys = nms._2.map(m => m.f1).zipWithIndex.map(di => di._2.toDouble -> di._1)
        new MemXYSeries(xys.map(_._1), xys.map(_._2), nms._1)
      })
      val f1Data = new XYData(f1Series: _*)
      val f1Chart = new XYChart("%s F1" format (pt), f1Data)
      f1Chart.y.label = "F1"
      f1Chart.showLegend = true
      GnuplotPlotter.pdf(f1Chart, outputDir, "f1_curve_" + pt.toLowerCase.replaceAll("\\s+", "_"))
    }
  }

  class Annotation(val tuple: List[String], val label: String, val correct: Boolean) {
    override def toString = "%s\t%s\t%s" format(if (correct) "1" else "0", label, tuple)

    def fact = tuple -> label
  }

  def readAnnotations(filename: String): Map[(List[String], String), Annotation] = {
    val annotations = new mutable.HashMap[(List[String], String), Annotation]()

    val file = new File(filename)
    val reader = new BufferedReader(new FileReader(file))
    var line: String = ""

    var cnt_entities = 0
    while ( {
      line = reader.readLine();
      line != null
    }) {
      var entityName: String = ""
      val iter = line.split("\t").toIterator
      val e: List[String] = if (iter.hasNext) {
        entityName = iter.next()
        List(entityName)
      } else null
      iter.foreach(relationName => {
        annotations(e -> relationName) = new Annotation(e, relationName, true)
      })

      cnt_entities += 1
      if (cnt_entities % 10000 == 0)
        println("[ ...loaded: " + cnt_entities + " entities... ]")
    }
    reader.close()
    annotations.toMap
  }

  def main(args: Array[String]) {
    val pathToPredictions = if (args.length > 0) args(0) else "data/out/latest/predict.txt"
    val pathToEvaluationOutput = if (args.length > 1) args(1) else Conf.outDir.getAbsolutePath
    val annotations = readAnnotations("data/figer/gold.csv")
    val rankFileNames = Seq(
      // new File("eval/unary/unary.rank.0.001.txt") -> "0.001",
      // new File("eval/unary/unary.rank.0.1-NF.txt") -> "NF",
      new File(pathToPredictions) -> "Ours",
      new File("data/figer/figer.csv") -> "Figer"
    )
    evaluate(rankFileNames, annotations.keys.toSet, pathToEvaluationOutput, 100)
  }
}

object LoadFIGER extends App {

  type SPDB = TensorKB

  /**
   * Load Figer training and test data
   * @param db
   * @param featureFilter whether to use a column as a observed feature (has a weight for each useFeatureFilter column)
   * @param predictFilter whether to use a column for matrix completion (has an embedding of size k)
   * @param useFeatureFilter whether to use the observed features for completion (has as many weights as observed features)
   * @param random
   * @return
   */
  def loadFigerData(db: SPDB,
                    featureFilter: String => Boolean = s => true,
                    predictFilter: String => Boolean = s => true,
                    useFeatureFilter: String => Boolean = s => true,
                    sampleTrainEnts: Double = Conf.getDouble("figer.sample-train-entities"),
                    sampleTrainFacts: Double = Conf.getDouble("figer.sample-train-facts"),
                    probDev: Double = Conf.getDouble("figer.prob-dev"),
                    probNegLabels: Double = Conf.getDouble("figer.prob-neg-labels")
                   )(implicit random: Random): Set[String] = {
    println(s"$db, ${Conf}, ${CellType}, $sampleTrainEnts, $sampleTrainFacts, $probDev")
    val labels = new mutable.HashSet[String]
    labels ++= readFigerData(db, Conf.getString("figer.dataDir") + "/train.pbf", CellType.Train, false,
      sampleTrainEnts,
      sampleTrainFacts,
      probDev,
      featureFilter, predictFilter, useFeatureFilter
    )
    labels ++= readFigerData(db, Conf.getString("figer.dataDir") + "/gold.pbf", CellType.Test, true,
      featureFilter = featureFilter, predictFilter = predictFilter, useFeatureFilter = useFeatureFilter)
    addMissingAnnotationsAsNegative(db, probNegLabels, _.startsWith("/"))
    println("[  Figer loaded with " + db.numCells + " cells, " + db.arg1s.size + " arg1s, " + db.arg2s.size + " arg2s, " + db.relations.size + " relations. ]")
    println(s"[  Labels: ${db.relations.filter(_.toString.startsWith("/")).size} ]")
    labels.toSet
  }

  // goes through the spdb, and adds all the cells for "labels" as negative
  // determines whether to treat as train or test depending on whether any cell is test
  def addMissingAnnotationsAsNegative(db: SPDB, prop: Double = 0.1, label: (String) => Boolean = _.startsWith("REL$"))(implicit random: Random = cc.factorie.random) {
    val labels = db.relations.filter(r => label(r.toString)).toSet
    for (e <- db.arg1s) {
      val labelFacts = db.getBy2(e).flatMap(r_e2 => {
        assert(r_e2._2 == DefaultIx)
        db.getFact(r_e2._1, e, r_e2._2)
      }).filter(f => labels(f.key1.toString))
      // if any is test, all negative are test, else use for training
      var factType = labelFacts.head.cellType
      for (f <- labelFacts) {
        if (f.cellType == CellType.Test) factType = CellType.Test
      }
      for (l <- labels) {
        if (db.getFact(l, e, DefaultIx).isEmpty && prop > random.nextDouble())
          db += Cell(l, e, DefaultIx, 0.0, factType)
      }
    }
  }

  def readFigerData(db: SPDB, filename: String, factType: CellType.CellType = CellType.Train, rowPerMention: Boolean,
                    probEntities: Double = 1.0, probFacts: Double = 1.0, probDev: Double = 0.0,
                    featureFilter: String => Boolean = s => true,
                    predictFilter: String => Boolean = s => true,
                    useFeatureFilter: String => Boolean = s => true)(implicit random: Random): Set[String] = {
    import FigerPB._
    println("Reading " + filename)
    val file = new File(filename)
    val fs = new FileInputStream(file)

    var cnt_entities = 0
    var mention: Option[Mention] = None

    val labels = new mutable.HashSet[String]
    while ( {
      mention = Mention.parseDelimitedFrom(fs);
      mention.isDefined
    }) {
      val entityName = mention.get.`entityName`.get
      val tupleString = if (rowPerMention) entityName + "_%03d" format (cnt_entities) else entityName
      if (random.nextDouble() < probEntities) {
        val e = tupleString

        val entityFactType = if (factType != CellType.Train || random.nextDouble() > probDev) factType else CellType.Dev
        mention.get.`labels`.foreach(relationName => {
          if (!relationName.startsWith("FINER_")) {
            val r = relationName
            db += Cell(r, e, DefaultIx, 1.0, entityFactType) // can be "dev"
            labels += relationName
          }
        })

        mention.get.`features`.foreach(relationName => {
          if (random.nextDouble() < probFacts) {
            if (predictFilter(relationName) || useFeatureFilter(relationName)) {
              val r = relationName
              db += Cell(r, e, DefaultIx, 1.0, CellType.Train) // these are never "dev" or "test"
            }
            if (featureFilter(relationName)) {
              // val idx = db.indexFeature(relationName)
              // db.features(e).update(idx, 1.0)
            }
          }
        })

        cnt_entities += 1
        if (cnt_entities % 10000 == 0)
          println("[ ...loaded: " + db.numCells + " cells, " + db.arg1s.size + " arg1s, " + db.relations.size + " relations. ]")
      }
    }
    println("[ Loaded: " + db.numCells + " cells, " + db.arg1s.size + " arg1s, " + db.relations.size + " relations. ]")
    fs.close()
    labels.toSet
  }

  def apply(k: Int = 100, subsample: Double = 1.0): TensorKB = {
    val kb = new TensorKB(k)
    implicit val random = new Random(0)

    //loading cells
    // TODO: currently ignores subsample
    val labels = loadFigerData(kb, s => false, s => true, s => false)

    //loading formulae
    if (Conf.hasPath("figer.formulaeFile") && Conf.getString("figer.formulaeFile") != "None") {
      val formulaePath = Conf.getString("figer.formulaeFile")
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

object WriteFIGER extends App {
  def apply(db: TensorDB, filePath: String = "./data/out/predict.txt"): Unit = {
    val writer = new FileWriter(filePath)
    val testCellsWithPrediction = db.testCells.map(c => {
      val r = c.key1
      val ep = c.key2

      val p = db.prob(r, ep)

      (c, p)
    }).sortBy(-_._2)


    testCellsWithPrediction.foreach { case (cell, p) =>
      val e = cell.key2.toString
      writer.write(s"$p\t$e\t" + "REL$NA" + s"\t${ cell.key1 }\n")
    }
    writer.close()
  }
}

