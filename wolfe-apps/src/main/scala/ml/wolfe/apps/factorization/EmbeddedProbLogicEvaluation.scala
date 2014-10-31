package ml.wolfe.apps.factorization

import java.io.File

import cc.factorie.la.DenseTensor1
import com.typesafe.config.ConfigFactory
import ml.wolfe.apps.factorization.FactorizationUtil.Row

import scala.collection.immutable.HashMap
import scala.collection.mutable
import scala.util.Random

/**
 * @author Sebastian Riedel
 */
object EmbeddedProbLogicEvaluation {

  def main(args: Array[String]) {
    implicit val conf = ConfigFactory.parseFile(new File("conf/epl.conf"))
    implicit val random = new Random(0)
    assert(!conf.entrySet().isEmpty, "Couldn't find configuration file.")

    def relationFilter(rel: String) = rel.startsWith("path") || (rel.startsWith("REL$") && rel != "REL$NA")

    //load raw data
    val trainRaw = FactorizationUtil.loadLiminFile(new File(conf.getString("epl.train")), relationFilter)
    val train = FactorizationUtil.filterRows(random.shuffle(trainRaw.toSeq), 5, 2)
    //val train = FactorizationUtil.filterRowsPairwise(trainRaw.toSeq, 50)

    val unlabeledRaw = FactorizationUtil.loadLiminFile(new File(conf.getString("epl.unlabeled")), relationFilter, skipUnlabeled = true)
    val unlabeled = FactorizationUtil.filterRows(unlabeledRaw.toSeq, 5, 2, !_.startsWith("REL$"))
    val combined = if (conf.getBoolean("epl.use-unlabeled")) train ++ unlabeled else train


    val trainRelations = combined.flatMap(_.relations.map(_._1)).distinct.sorted // REL$/book/book_edition/author_editor

    val freebaseRelations = Seq("REL$/business/person/company")//trainRelations.filter(_.startsWith("REL$"))
    val surfacePatterns = trainRelations.filterNot(_.startsWith("REL$")).toSet

    val testRaw = FactorizationUtil.loadLiminFile(new File(conf.getString("epl.test")), relationFilter,
      skipUnlabeled = true, minObsCount = 1).toSeq
    println(testRaw.find(_.arg1 == "Arthur Mann"))
    val test = FactorizationUtil.filterRows(testRaw, 1, 1, surfacePatterns)
    println(test.find(_.arg1 == "Arthur Mann"))



    println(trainRelations.size)
    println(train.size)
    println(unlabeled.size)
    println(test.size)

    println("Extracting rules")
    val trainRulesRaw = RuleLearner.learn(combined)
    val cooccurMin = conf.getDouble("epl.min-cooccur")

    println("Finding components")
    val connected = RuleFilters.connectedComponents(trainRulesRaw, cooccurMin)
    val connectedFreebase = connected.filter(c => freebaseRelations.exists(c._1.nodes))
    println(s"Connected Components: ${ connected.size }")
    println(s"Connected Components with freebase relations: ${ connectedFreebase.size }")
    println(s"Total count of rules in components: ${connectedFreebase.view.map(_._2.rules2.size).sum}")
    FactorizationUtil.saveToFile(
      connected.map(_._1.nodes.toList.sorted.mkString("------\n  ", "\n  ","")),
      new File("/tmp/components.txt"))


    val trainRules = Rules(connectedFreebase.map(_._2.rules2).reduce(_ ++ _))//RuleFilters.keep2ndOrder(joinedRulesRaw, cooccurMin)

    println(s"Original rule count: ${ trainRulesRaw.rules2.size }")
    println(s"Filtered rule count: ${ trainRules.rules2.size }")


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
    val learnedRules = ple.pairwiseRules(trainRules.rules2.keys)
    compareRules(learnedRules,trainRules.rules2)

  }

  def compareRules(rules2:Map[(String,String),Rule2],rules1:Map[(String,String),Rule2]) = {
    val paired = for (r1 <- rules1.values; r2 <- rules2.get(r1.rel1 -> r1.rel2)) yield (r1, r2, r2.prob1given2Inc(r1))
    val printPaired = paired.toSeq.sortBy(-_._3).view.map(t => s"Mismatch: ${ t._3 }\n${ t._1 }\n${ t._2 }")
    val printPairedInv = paired.toSeq.sortBy(_._3).view.map(t => s"Mismatch: ${ t._3 }\n${ t._1 }\n${ t._2 }")
    FactorizationUtil.saveToFile(printPaired, new File("/tmp/rule-comparisons.txt"))
    FactorizationUtil.saveToFile(printPairedInv, new File("/tmp/rule-comparisons-inv.txt"))

    val avgCondMismatch = paired.view.map(t => math.abs(t._3)).sum / paired.size
    println("Average cond. mismatch: " + avgCondMismatch)

  }

}

object EmbeddedProbLogicPlayground {

  import math._

  def manualRules(): Unit = {
    implicit val conf = ConfigFactory.parseFile(new File("conf/epl-synth.conf")) withFallback ConfigFactory.parseFile(new File("conf/epl.conf"))
    implicit val random = new Random(0)


    val test = FactorizationUtil.sampleRows(10, 10, 0.2)
    val manualData = Seq(
      Row("e1","e2",Seq("r1","r2").map(_ -> 1.0)),
      Row("e3","e4",Seq("r2","r3").map(_ -> 1.0)),
      Row("e5","e6",Seq("r4","r5").map(_ -> 1.0))
    )

    val dataRelations = test.flatMap(_.observedTrue).distinct.sorted


    val manualEmbeddings = ProbLogicEmbeddings(Map(
      "r0" -> PredicateEmbedding("r0", new DenseTensor1(Array(1.0, 0.0)), 1.0, -2.0, 10.0),
      "r1" -> PredicateEmbedding("r1", new DenseTensor1(Array(1.0 / sqrt(2), 1.0 / sqrt(2))), 1.0, 0.0, 1.0)
    ))
    val ple = manualEmbeddings //ProbLogicEmbedder.embed(manualRules).copy(average = false) //ProbLogicEmbedder.embed(randomRules)

    val rulesData = RuleLearner.learn(manualData)

    val pleData = ProbLogicEmbedder.embed(rulesData)
    val learnedRules = pleData.pairwiseRules(rulesData.rules2.keys)

    EmbeddedProbLogicEvaluation.compareRules(learnedRules,rulesData.rules2)

    val predictionsData = for (row <- test) yield {
      row.copy(relations = dataRelations.map(r => r -> pleData.predict(row.observedTrue, r)))
    }

    println(FactorizationUtil.renderPredictions(predictionsData, test))
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

  def keep2ndOrder(rules: Rules,
                   minCooccurCount: Double) = {
    val filtered = rules.rules2.filter(_._2.cooccurCount >= minCooccurCount - 0.0001).map(p => p._1 -> p._2.count * p._2.probs(true, true))
    val graph = filtered ++ filtered.map(p => p.copy(_1 = p._1.swap)) withDefaultValue 0.0
    val arg1ToEdges = filtered.toList.groupBy(_._1._1) withDefaultValue Nil
    val arg2ToEdges = filtered.toList.groupBy(_._1._2) withDefaultValue Nil

    def expand(graph: Map[(String, String), Double]) = {
      //go over all edges (e1,e2) and connect e1 to e3 for each (e2,e3)
      //todo: this doesn't find the highest scoring path though
      val newEdges = for (((arg1, arg2), s1) <- graph;
                          ((_, arg3), s2) <- arg1ToEdges(arg2)
                          if arg3 != arg1 && !graph.contains((arg1, arg3))) yield (arg1, arg3) -> min(s1, s2)
      graph ++ newEdges
    }
    val expanded = expand(graph)
    rules.copy(rules2 = rules.rules2.filterKeys(expanded.contains))
  }

  class Component(first:String) {
    val edges = new mutable.HashSet[(String, String)]
    val nodes = new mutable.HashSet[String]
    nodes += first
  }

  def connectedComponents(rules: Rules, minCooccurCount: Double, filter:String =>Boolean = _ => true) = {
    val filtered = rules.rules2.filter(_._2.cooccurCount >= minCooccurCount - 0.0001).map(p => p._1 -> p._2.count * p._2.probs(true, true))
    val graph = filtered ++ filtered.map(p => p.copy(_1 = p._1.swap)) withDefaultValue 0.0

    val components = new mutable.HashMap[String, Component]()
    for ((a1, a2) <- graph.keys) {
      val c1 = components.getOrElseUpdate(a1, new Component(a1))
      val c2 = components.getOrElseUpdate(a2, new Component(a2))
      if (c1 == c2) {
        c1.edges += ((a1,a2))
      } else {
        val (keep,discard) = if (c1.nodes.size > c2.nodes.size) (c1,c2) else (c2,c1)
        keep.edges += ((a1,a2))
        keep.edges ++= discard.edges
        keep.nodes ++= discard.nodes
        for (n <- discard.nodes) components(n) = keep
      }
    }
    val filteredComponents = components.values.toList.distinct.view.filter(c => c.nodes.exists(filter))
    filteredComponents.map(c => c -> rules.copy(rules2 = rules.rules2.filterKeys(e => c.nodes(e._1) && c.nodes(e._2)))).toList

  }


}



