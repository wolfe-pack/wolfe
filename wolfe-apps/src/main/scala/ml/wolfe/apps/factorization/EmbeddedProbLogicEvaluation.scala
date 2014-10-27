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
    val trainRaw = FactorizationUtil.loadLiminFile(new File(conf.getString("epl.train")), relationFilter).take(2000)
    //val train = FactorizationUtil.filterRows(trainRaw.toSeq, 10, 2)
    val train = FactorizationUtil.filterRowsPairwise(trainRaw.toSeq, 50)
    val trainRelations = train.flatMap(_.relations.map(_._1)).distinct.sorted
    val freebaseRelations = trainRelations.filter(_.startsWith("REL$"))

    val testRaw = FactorizationUtil.loadLiminFile(new File(conf.getString("epl.test")), relationFilter)
    val test = FactorizationUtil.filterRows(testRaw.toSeq, 1, 1, trainRelations.filterNot(_.startsWith("REL$")).toSet)

    println(trainRelations.size)
    println(train.size)
    println(test.size)

    println("Extracting rules")
    val trainRules = RuleLearner.learn(train)
    FactorizationUtil.saveToFile(trainRules.rules1.values.toSeq.sortBy(_.rel).mkString("\n"), new File("/tmp/rules1.txt"))
    FactorizationUtil.saveToFile(trainRules.rules2.values.toSeq.sortBy(-_.probs(true,true)).mkString("\n"), new File("/tmp/rules2.txt"))


    println("Embedding rules")
    val ple = ProbLogicEmbedder.embed(trainRules)

    println("Prediction")
    val predictedRows = test map (row => ple.predictRow(row, freebaseRelations))
    val predictedFacts = FactorizationUtil.toRankedFacts(test zip predictedRows)

    println(predictedFacts.take(100).mkString("\n"))
    FactorizationUtil.saveForUSchemaEval(predictedFacts, new File("/tmp/ple.txt"))
    FactorizationUtil.saveToFile(predictedFacts.mkString("\n"), new File("/tmp/ranked.txt"))



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
        ("r0", "r1") -> Rule2("r0", "r1", ruleProb),
        ("r1", "r2") -> Rule2("r1", "r2", ruleProb)
        //      ("r2", "r3") -> Rule("r2", "r3", ruleProb)
      ),
      Map(
        "r0" -> Rule1("r0", 0.1),
        "r1" -> Rule1("r1", 0.1),
        "r2" -> Rule1("r1", 0.1)

      )
    )
    val test = FactorizationUtil.sampleRows(10, 3, 0.2)


    val ruleRelations = manualRules.rules2.values.flatMap(r => Seq(r.rel1, r.rel2)).toSeq.distinct.sorted
    val dataRelations = test.flatMap(_.observedTrue).distinct.sorted


    val manualEmbeddings = ProbLogicEmbeddings(Map(
      "r0" -> PredicateEmbedding("r0", new DenseTensor1(Array(1.0, 0.0)), 1.0, -2.0, 10.0),
      "r1" -> PredicateEmbedding("r1", new DenseTensor1(Array(1.0 / sqrt(2), 1.0 / sqrt(2))), 1.0, 0.0, 1.0)
    ))
    val ple = manualEmbeddings//ProbLogicEmbedder.embed(manualRules).copy(average = false) //ProbLogicEmbedder.embed(randomRules)


    val rulesData = RuleLearner.learn(test)

    val pleData = ProbLogicEmbedder.embed(rulesData)

    val predictionsRules = for (row <- test) yield {
      row.copy(relations = ruleRelations.map(r => r -> ple.predict(row.observedTrue, r)))
    }
    val predictionsData = for (row <- test) yield {
      row.copy(relations = dataRelations.map(r => r -> pleData.predict(row.observedTrue, r)))
    }

    println(FactorizationUtil.renderPredictions(predictionsData, test))
    println(FactorizationUtil.renderPredictions(predictionsRules, test))
    println(pleData.embeddings.values.mkString("\n"))

  }

  def main(args: Array[String]) {
    manualRules()
  }

  def rulesFromRandomData() {
    implicit val conf = ConfigFactory.parseFile(new File("conf/epl-synth.conf")) withFallback ConfigFactory.parseFile(new File("conf/epl.conf"))
    implicit val random = new Random(0)

    val randomRows = FactorizationUtil.sampleRows(10, 4, 0.2)
    val randomRelations = randomRows.flatMap(_.relations.map(_._1)).distinct.sorted
    val randomRules = RuleLearner.learn(randomRows)

    val ple = ProbLogicEmbedder.embed(randomRules)

    val predictions = for (row <- randomRows) yield {
      row.copy(relations = randomRelations.map(r => r -> ple.predict(row.observedTrue, r)))
    }

    println(randomRules)

    println(FactorizationUtil.renderPredictions(predictions, randomRows))

  }
}




