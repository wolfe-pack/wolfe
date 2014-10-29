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
    implicit val random = new Random()
    assert(!conf.entrySet().isEmpty, "Couldn't find configuration file.")

    def relationFilter(rel: String) = rel.startsWith("path") || (rel.startsWith("REL$") && rel != "REL$NA")

    //load raw data
    val trainRaw = FactorizationUtil.loadLiminFile(new File(conf.getString("epl.train")), relationFilter).take(10000)
    val train = FactorizationUtil.filterRows(trainRaw.toSeq, 5, 2)
    //val train = FactorizationUtil.filterRowsPairwise(trainRaw.toSeq, 50)
    val trainRelations = train.flatMap(_.relations.map(_._1)).distinct.sorted
    val freebaseRelations = trainRelations.filter(_.startsWith("REL$"))
    val surfacePatterns = trainRelations.filterNot(_.startsWith("REL$")).toSet

    val testRaw = FactorizationUtil.loadLiminFile(new File(conf.getString("epl.test")), relationFilter, skipUnlabeled = true)
    val test = FactorizationUtil.filterRows(testRaw.toSeq, 1, 1, surfacePatterns)
    val unlabeledRaw = FactorizationUtil.loadLiminFile(new File(conf.getString("epl.unlabeled")), relationFilter, skipUnlabeled = true)
    val unlabeled = FactorizationUtil.filterRows(unlabeledRaw.toSeq, 1, 2, surfacePatterns)

    println(trainRelations.size)
    println(train.size)
    println(unlabeled.size)
    println(test.size)

    println("Extracting rules")
    val trainRulesRaw = RuleLearner.learn(train)
    val joinedRulesRaw = conf.getBoolean("epl.use-unlabeled") match {
      case true => trainRulesRaw + RuleLearner.learn(unlabeled)
      case false => trainRulesRaw
    }
    val trainRules = RuleFilters.keep2ndOrder(joinedRulesRaw)

    println(s"Original rule count: ${joinedRulesRaw.rules2.size}")
    println(s"Filtered rule count: ${trainRules.rules2.size}")


    FactorizationUtil.saveToFile(trainRules.rules1.values.toSeq.sortBy(_.rel).mkString("\n"), new File("/tmp/rules1.txt"))
    FactorizationUtil.saveToFile(trainRules.rules2.values.toSeq.sortBy(-_.probs(true, true)), new File("/tmp/rules2.txt"))


    println(s"Embedding ${ trainRules.rules2.size } rules")
    val ple = ProbLogicEmbedder.embed(trainRules)

    println("Prediction")
    val predictedRows = test map (row => ple.predictRow(row, freebaseRelations))
    val predictedFacts = FactorizationUtil.toRankedFacts(test zip predictedRows).filter(_.score > 0.0)

    println(predictedFacts.take(100).mkString("\n"))
    FactorizationUtil.saveForUSchemaEval(predictedFacts, new File("/tmp/ple.txt"))
    FactorizationUtil.saveToFile(predictedFacts.mkString("\n"), new File("/tmp/ranked.txt"))

    println("Extracting learned rules")
    val learnedRules = ple.pairwiseRules
    val paired = for (r1 <- trainRules.rules2.values; r2 <- learnedRules.get(r1.rel1 -> r1.rel2)) yield (r1,r2,r2.prob1given2Inc(r1))
    val printPaired = paired.toSeq.sortBy(-_._3).view.map(t => s"Mismatch: ${t._3}\n${t._1}\n${t._2}")
    val printPairedInv = paired.toSeq.sortBy(_._3).view.map(t => s"Mismatch: ${t._3}\n${t._1}\n${t._2}")
    FactorizationUtil.saveToFile(printPaired,new File("/tmp/rule-comparisons.txt"))
    FactorizationUtil.saveToFile(printPairedInv,new File("/tmp/rule-comparisons-inv.txt"))

    val avgCondMismatch = paired.view.map(t => math.abs(t._3)).sum / paired.size
    println("Average cond. mismatch: " + avgCondMismatch)

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
    val test = FactorizationUtil.sampleRows(10, 10, 0.2)


    val ruleRelations = manualRules.rules2.values.flatMap(r => Seq(r.rel1, r.rel2)).toSeq.distinct.sorted
    val dataRelations = test.flatMap(_.observedTrue).distinct.sorted


    val manualEmbeddings = ProbLogicEmbeddings(Map(
      "r0" -> PredicateEmbedding("r0", new DenseTensor1(Array(1.0, 0.0)), 1.0, -2.0, 10.0),
      "r1" -> PredicateEmbedding("r1", new DenseTensor1(Array(1.0 / sqrt(2), 1.0 / sqrt(2))), 1.0, 0.0, 1.0)
    ))
    val ple = manualEmbeddings //ProbLogicEmbedder.embed(manualRules).copy(average = false) //ProbLogicEmbedder.embed(randomRules)


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

object RuleFilters {

  import math._

  def keep2ndOrder(rules: Rules) = {
    val filtered = rules.rules2.filter(_._2.probs(true, true) > 0.0).map(p => p._1 -> p._2.probs(true, true))
    val graph = filtered ++ filtered.map(p => p.copy(_1 = p._1.swap)) withDefaultValue 0.0
    val arg1ToEdges = filtered.toList.groupBy(_._1._1) withDefaultValue Nil
    val arg2ToEdges = filtered.toList.groupBy(_._1._2) withDefaultValue Nil

    def expand(graph: Map[(String, String), Double]) = {
      //go over all edges (e1,e2) and connect e1 to e3 for each (e2,e3)
      val newEdges = for (((arg1, arg2), s1) <- graph;
                          ((_, arg3), s2) <- arg1ToEdges(arg2)
                          if arg3 != arg1 && !graph.contains((arg1, arg3))) yield (arg1, arg3) -> min(s1, s2)
      graph ++ newEdges
    }
    val expanded = expand(graph)
    rules.copy(rules2 = rules.rules2.filterKeys(expanded.contains))
  }
}



