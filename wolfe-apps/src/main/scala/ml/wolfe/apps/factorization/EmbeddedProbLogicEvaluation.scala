package ml.wolfe.apps.factorization

import java.io.File

import cc.factorie.la.DenseTensor1
import com.typesafe.config.ConfigFactory

import scala.util.Random

/**
 * @author Sebastian Riedel
 */
object EmbeddedProbLogicEvaluation {

  def main(args: Array[String]) {
    implicit val conf = ConfigFactory.parseFile(new File("conf/epl.conf"))

    def relationFilter(rel: String) = rel.startsWith("path") || (rel.startsWith("REL$") && rel != "REL$NA")

    //load raw data
    val trainRaw = FactorizationUtil.loadLiminFile(new File(conf.getString("epl.train")), relationFilter).take(10000)
    val train = FactorizationUtil.filterRows(trainRaw.toSeq, 10, 2)
    val trainRelations = train.flatMap(_.relations.map(_._1)).distinct.sorted
    val freebaseRelations = trainRelations.filter(_.startsWith("REL$"))

    val testRaw = FactorizationUtil.loadLiminFile(new File(conf.getString("epl.test")), relationFilter)
    val test = FactorizationUtil.filterRows(testRaw.toSeq, 0, 0, trainRelations.filterNot(_.startsWith("REL$")).toSet)

    println(trainRelations.size)
    println(train.size)
    println(test.size)

    println("Extracting rules")
    val trainRules = RuleLearner.pairwiseRules(train)

    println("Embedding rules")
    val ple = ProbLogicEmbedder.embed(trainRules)

    println("Prediction")
    val predictedRows = test map (row => ple.predictRow(row, freebaseRelations))
    val predictedFacts = FactorizationUtil.toRankedFacts(test zip predictedRows)

    println(predictedFacts.take(100).mkString("\n"))
    FactorizationUtil.saveForUSchemaEval(predictedFacts, new File("/tmp/ple.txt"))


  }

}

object EmbeddedProbLogicPlayground {

  import math._

  def manualRules(): Unit = {
    implicit val conf = ConfigFactory.parseFile(new File("conf/epl-synth.conf")) withFallback ConfigFactory.parseFile(new File("conf/epl.conf"))
    implicit val random = new Random(0)

    val ruleProb = Map(
      (true, true) -> 0.1,
      (true, false) -> 0.0,
      (false, true) -> 0.1,
      (false, false) -> 0.8
    )

    val manualRules = Rules(
      Map(
        ("r0", "r1") -> Rule2("r0", "r1", ruleProb)
        //("r1", "r2") -> Rule2("r1", "r2", ruleProb),
        //      ("r2", "r3") -> Rule("r2", "r3", ruleProb)
      ),
      Map(
        "r0" -> Rule1("r0", 0.1),
        "r1" -> Rule1("r1", 0.1)
      )
    )

    val allRelations = manualRules.rules2.values.flatMap(r => Seq(r.rel1, r.rel2)).toSeq.distinct.sorted

    val ple = ProbLogicEmbedder.embed(manualRules).copy(average = false) //ProbLogicEmbedder.embed(randomRules)

    val manualEmbeddings = ProbLogicEmbeddings(Map(
      "r0" -> PredicateEmbedding("r0", new DenseTensor1(Array(1.0, 0.0)), 1.0, -2.0, 10.0),
      "r1" -> PredicateEmbedding("r1", new DenseTensor1(Array(1.0 / sqrt(2), 1.0 / sqrt(2))), 1.0, 0.0, 1.0)
    ))

    val test = FactorizationUtil.sampleRows(10, 2, 0.2)

    val predictions = for (row <- test) yield {
      row.copy(relations = allRelations.map(r => r -> ple.predict(row.observedTrue, r)))
    }
    val manualPredictions = for (row <- test) yield {
      row.copy(relations = allRelations.map(r => r -> manualEmbeddings.predict(row.observedTrue, r)))
    }

    println(FactorizationUtil.renderPredictions(manualPredictions, test))
    println(FactorizationUtil.renderPredictions(predictions, test))
    println(ple.embeddings.values.mkString("\n"))

  }

  def main(args: Array[String]) {
    manualRules()
  }

  def rulesFromRandomData() {
    implicit val conf = ConfigFactory.parseFile(new File("conf/epl-synth.conf")) withFallback ConfigFactory.parseFile(new File("conf/epl.conf"))
    implicit val random = new Random(0)

    val randomRows = FactorizationUtil.sampleRows(10, 4, 0.2)
    val randomRelations = randomRows.flatMap(_.relations.map(_._1)).distinct.sorted
    val randomRules = RuleLearner.pairwiseRules(randomRows)

    val ple = ProbLogicEmbedder.embed(randomRules)

    val predictions = for (row <- randomRows) yield {
      row.copy(relations = randomRelations.map(r => r -> ple.predict(row.observedTrue, r)))
    }

    println(randomRules)

    println(FactorizationUtil.renderPredictions(predictions, randomRows))

  }
}




