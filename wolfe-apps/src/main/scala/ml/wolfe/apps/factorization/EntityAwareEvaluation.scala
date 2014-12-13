package ml.wolfe.apps.factorization

import java.io.File

import com.typesafe.config.ConfigFactory
import ml.wolfe.Wolfe._
import ml.wolfe.apps.factorization.FactorizationUtil.Row

import scala.collection.mutable
import scala.util.Random

/**
 * @author Sebastian Riedel
 */
object EntityAwareEvaluation {

  case class Entity(entity: Any, counts: Map[String, Double]) {
    override def toString = {
      val sorted = counts.toSeq.sortBy(-_._2).map(p => p._1 + " " + p._2)
      s"""
        |-----
        |$entity
        |${ sorted.mkString("  ", "\n  ", "") }
      """.stripMargin
    }

    def asArg1 = counts.keys.map(_ + "#1")
    def asArg2 = counts.keys.map(_ + "#2")


  }

  def unaryToBinary(unary: String) = unary.substring(3, unary.length - 2)

  def entitiesFromRows(rows: Seq[Row]) = {
    val result = new mutable.HashMap[Any, mutable.HashMap[String, Double]]
    for (row <- rows) {
      val arg1Counts = result.getOrElseUpdate(row.arg1, new mutable.HashMap[String, Double]())
      val arg2Counts = result.getOrElseUpdate(row.arg2, new mutable.HashMap[String, Double]())

      for ((rel, value) <- row.relations) {
        val a1 = "A1#" + rel
        val a2 = "A2#" + rel
        arg1Counts(a1) = arg1Counts.getOrElse(a1, 0.0) + value
        arg2Counts(a2) = arg1Counts.getOrElse(a2, 0.0) + value
      }
    }
    result.map(p => p._1 -> Entity(p._1, p._2.toMap)).toMap
  }

  def joinRules(rules: Seq[Rules]) = {

    val result = new mutable.HashMap[(String, String), Rule2]
    val singleCounts = new mutable.HashMap[String, Double] withDefaultValue 0.0
    for (ruleMap <- rules.view.map(_.rules2)) {
      for (((r1, r2), rule) <- ruleMap) {
        result.get((r1, r2)) match {
          case Some(oldRule) =>
            result((r1, r2)) = oldRule + rule
          case None =>
            result((r1, r2)) = rule //todo: we should use updated single counts if seen in previous rule maps
        }
      }
    }
    Rules(result.toMap)
  }

  import EmbeddedProbLogicEvaluation._

  def main(args: Array[String]) {
    implicit val conf = ConfigFactory.parseFile(new File("conf/epl-ent.conf"))
    implicit val random = new Random(0)
    assert(!conf.entrySet().isEmpty, "Couldn't find configuration file.")

    val subsample = conf.getDouble("epl.subsample")
    val priorRepulsion = conf.getDouble("epl.prior-repulsion")
    val cooccurMin = conf.getDouble("epl.min-cooccur")



    def relationFilter(rel: String) = rel.startsWith("path") || (rel.startsWith("REL$") && rel != "REL$NA")

    //load raw data
    val trainRaw = FactorizationUtil.loadLiminFile(new File(conf.getString("epl.train")), relationFilter)
    val train = FactorizationUtil.filterRows(random.shuffle(trainRaw.toBuffer), conf.getInt("epl.min-rows"), conf.getInt("epl.min-cols"))

    val unlabeledRaw = FactorizationUtil.loadLiminFile(new File(conf.getString("epl.unlabeled")), relationFilter, skipUnlabeled = true)
    val unlabeled = FactorizationUtil.filterRows(unlabeledRaw.toSeq, conf.getInt("epl.min-rows"), conf.getInt("epl.min-cols"), !_.startsWith("REL$"))
    val combined = if (conf.getBoolean("epl.use-unlabeled")) train ++ unlabeled else train


    //relations
    val trainRelations = combined.flatMap(_.relations.map(_._1)).distinct.sorted // REL$/book/book_edition/author_editor
    val freebaseRelations = trainRelations.filter(_.startsWith("REL$")) //Seq("REL$/business/person/company")//
    val surfacePatterns = trainRelations.filterNot(_.startsWith("REL$")).toSet

    val testRaw = FactorizationUtil.loadLiminFile(new File(conf.getString("epl.test")), relationFilter,
      skipUnlabeled = true, minObsCount = 1).toSeq
    val test = FactorizationUtil.filterRows(testRaw, 1, 1, surfacePatterns)

    println(trainRelations.size)
    println(train.size)
    println(unlabeled.size)
    println(test.size)

    println("Extracting entities")
    val entities = entitiesFromRows(train ++ unlabeled)
    FactorizationUtil.saveToFile(entities.values.toSeq.sortBy(_.entity.toString), new File("/tmp/entities.txt"))
    //val filteredEntities = entities.mapValues(e => e.copy(counts = e.counts.toSeq.sortBy(-_._2).take(5).toMap))
    val filteredEntities = Map() ++ entities.mapValues(e => e.copy(counts = random.shuffle(e.counts.toSeq).take(20).toMap))
    FactorizationUtil.saveToFile(filteredEntities.values.toSeq.sortBy(_.entity.toString), new File("/tmp/filtered-entities.txt"))
    val testEntities = entitiesFromRows(test)
    FactorizationUtil.saveToFile(testEntities.values.toSeq.sortBy(_.entity.toString), new File("/tmp/test-entities.txt"))



    val priorCounts = Map((true, false) -> priorRepulsion, (false, true) -> priorRepulsion) withDefaultValue 0.0

    println("Extracting Binary rules")
    val trainRulesRaw =
      if (conf.getBoolean("epl.combine-datasets")) RuleLearner.learn(combined, priorCounts)
      else RuleLearner.learn(train, priorCounts) + RuleLearner.learn(unlabeled, priorCounts)

    println("Extracting Unary rules")
    val rulesUnary = EntityRuleLearner.extractUnaryRules(filteredEntities, subSample = 0.01)
    FactorizationUtil.saveToFile(rulesUnary.rules2.values.toArray.sortBy(-_.probs(true, true)), new File("/tmp/unary.txt"))
    println("Extracting Unary-Binary rules")
    //val rulesUnary2BinaryTrain = EntityRuleLearner.extractRel2UnaryRules(filteredEntities, train, subSample = 0.01)
    val rulesUnary2BinaryCombined = EntityRuleLearner.extractRel2UnaryRules(filteredEntities, train ++ unlabeled, subSample = 0.01)

    //println(rulesUnary2BinaryTrain.rules2.get("A1#path#nsubj|<-nsubj<-have->dobj->|dobj:INV#2" -> "path#nn|<-nn<-station->prep->in->pobj->|pobj"))
    //val rulesUnary2BinaryUnlabeled = EntityRuleLearner.extractRel2UnaryRules(filteredEntities, unlabeled, subSample = 0.01)
    //println(rulesUnary2BinaryUnlabeled.rules2.get("A1#path#nsubj|<-nsubj<-have->dobj->|dobj:INV#2" -> "path#nn|<-nn<-station->prep->in->pobj->|pobj"))
    //    val joined = joinRules(Seq(trainRulesRaw,rulesUnary,rulesUnary2BinaryTrain,rulesUnary2BinaryUnlabeled))//trainRulesRaw + rulesUnary + rulesUnary2BinaryTrain + rulesUnary2BinaryUnlabeled
    val joined = joinRules(Seq(trainRulesRaw, rulesUnary, rulesUnary2BinaryCombined)) //trainRulesRaw + rulesUnary + rulesUnary2BinaryTrain + rulesUnary2BinaryUnlabeled
    println("unary+binary: " + joined.rules2.size)
    FactorizationUtil.saveToFile(joined.rules2.values.toSeq.sortBy(-_.cond1given2), new File("/tmp/unary-binary.txt"))
    println(joined.rules2.get("A1#path#nsubj|<-nsubj<-have->dobj->|dobj:INV#2" -> "path#nn|<-nn<-station->prep->in->pobj->|pobj"))


    val trainRules = Rules(joined.rules2.filter(p => p._2.cooccurCount >= 1 || random.nextDouble() < subsample))
    //val trainRulesFiltered = trainRules.copy(rules2 = trainRules.)

    println(s"Original rule count: ${ joined.rules2.size }")
    println(s"Filtered rule count: ${ trainRules.rules2.size }")


    FactorizationUtil.saveToFile(trainRules.rules2.values.toSeq.sortBy(-_.cond1given2), new File("/tmp/ent-rules2.txt"))

    println(s"Embedding ${ trainRules.rules2.size } rules")
    val ple = ProbLogicEmbedder.embed(trainRules)

    println("Prediction")

    val predictor = new EntityAwarePredictor(ple, testEntities)
    val predictedFacts = test flatMap (row => predictor.predictAll(row, freebaseRelations))

    FactorizationUtil.saveToFile(predictedFacts.sortBy(-_.fact.score), new File("/tmp/ent-facts.txt"))

    if (conf.getBoolean("epl.print-comparisons")) {
      println("Extracting learned rules")
      val learnedRules = ple.pairwiseRules(trainRules.rules2.keys)
      EmbeddedProbLogicEvaluation.compareRules(learnedRules, trainRules.rules2)
    }
  }

}

object EntityRuleLearner {

  import ml.wolfe.apps.factorization.EntityAwareEvaluation._

  def toRule(rel1: String, rel2: String,
             pairCount: Int, singleCount1: Int, singleCount2: Int, normalizer: Double,
             priorCounts: Map[(Boolean, Boolean), Double] = Map.empty withDefaultValue 0.0) = {
    val prob11 = (pairCount + priorCounts(true, true)) / normalizer
    val prob10 = ((singleCount1 - pairCount) + priorCounts(true, false)) / normalizer
    val prob01 = ((singleCount2 - pairCount) + priorCounts(false, true)) / normalizer
    val prob00 = 1.0 - prob11 - prob10 - prob01
    val probs = Map(
      (true, true) -> prob11, (true, false) -> prob10,
      (false, true) -> prob01, (false, false) -> prob00
    )
    Rule2(rel1, rel2, probs, 1.0, count = normalizer,
      cond1given2 = prob11 / (prob01 + prob11),
      cond2given1 = prob11 / (prob10 + prob11))
  }

  def extractUnaryRules(entities: Map[Any, Entity],
                        subSample: Double = 1.0,
                        priorCounts: Map[(Boolean, Boolean), Double] = Map.empty withDefaultValue 0.0) = {

    val pairCountsArg1 = mutable.HashMap[(String, String), Int]() withDefaultValue 0
    val pairCountsArg2 = mutable.HashMap[(String, String), Int]() withDefaultValue 0
    val singleCountsInArg1 = mutable.HashMap[String, Int]() withDefaultValue 0
    val singleCountsInArg2 = mutable.HashMap[String, Int]() withDefaultValue 0

    println("Entities: " + entities.size)
    for (ent <- entities.values) {
      for (p <- ent.asArg1) singleCountsInArg1(p) += 1
      for (p <- ent.asArg2) singleCountsInArg2(p) += 1
      if (ent.entity == "Nevada") {
        println(ent.asArg1.mkString(","))
      }
      if (ent.entity == "OPEC") {
        println(ent.asArg1.mkString(","))
      }
      if (ent.asArg1.contains("A1#path#nn|<-nn<-secretary->appos->|appos#1")) {
        println(ent.entity)
        println("Blah: " + singleCountsInArg1("A1#path#nn|<-nn<-secretary->appos->|appos#1"))
      }
      for (p1 <- ent.asArg1; p2 <- ent.asArg1; if p1 != p2) {
        pairCountsArg1(p1 -> p2) += 1
      }
      for (p1 <- ent.asArg2; p2 <- ent.asArg2; if p1 != p2) {
        pairCountsArg2(p1 -> p2) += 1
      }
    }
    println("Done counting")
    val arg1s = singleCountsInArg1.keys.toArray.sorted
    val arg2s = singleCountsInArg2.keys.toArray.sorted
    val normalizer = entities.size.toDouble + priorCounts.values.sum
    val result = new mutable.HashMap[(String, String), Rule2]()
    println("Done sorting etc.")
    for (i1 <- 0 until arg1s.size; i2 <- i1 + 1 until arg1s.size) {
      val a1 = arg1s(i1)
      val a2 = arg1s(i2)
      if (a2 == "A1#path#nn|<-nn<-secretary->appos->|appos#1") {
        println(toRule(a1, a2, pairCountsArg1(a1 -> a2), singleCountsInArg1(a1), singleCountsInArg1(a2), normalizer, priorCounts))
      }
      if (pairCountsArg1(a1, a2) >= 1 || random.nextDouble() < subSample)
        result(a1 -> a2) = toRule(a1, a2, pairCountsArg1(a1 -> a2), singleCountsInArg1(a1), singleCountsInArg1(a2), normalizer, priorCounts)
    }
    for (i1 <- 0 until arg2s.size; i2 <- i1 + 1 until arg2s.size) {
      val a1 = arg2s(i1)
      val a2 = arg2s(i2)
      if (a2 == "A1#path#nn|<-nn<-secretary->appos->|appos#2") {
        println(toRule(a1, a2, pairCountsArg2(a1 -> a2), singleCountsInArg2(a1), singleCountsInArg2(a2), normalizer, priorCounts))
      }

      if (pairCountsArg2(a1, a2) >= 1 || random.nextDouble() < subSample)
        result(a1 -> a2) = toRule(a1, a2, pairCountsArg2(a1 -> a2), singleCountsInArg2(a1), singleCountsInArg2(a2), normalizer, priorCounts)
    }
    println("Done!")
    Rules(result.toMap)
  }


  def extractRel2UnaryRules(entities: Map[Any, Entity],
                            rows: Seq[Row],
                            subSample: Double = 1.0,
                            priorCounts: Map[(Boolean, Boolean), Double] = Map.empty withDefaultValue 0.0) = {

    val pairCounts = mutable.HashMap[(String, String), Int]() withDefaultValue 0
    val singleCounts = mutable.HashMap[String, Int]() withDefaultValue 0
    val singleCountsArgs = mutable.HashMap[String, Int]() withDefaultValue 0


    for (row <- rows) {
      val cells = row.relations
      val arg1 = entities(row.arg1)
      val arg2 = entities(row.arg2)
      for (cell <- cells) singleCounts(cell._1) += 1
      for (a1 <- arg1.asArg1) singleCountsArgs(a1) += 1
      for (a2 <- arg2.asArg2) singleCountsArgs(a2) += 1

      //we should avoid rules between unary and binary relations that are
      for ((rel, _) <- cells; a1 <- arg1.asArg1 if unaryToBinary(a1) != rel) {
        pairCounts(rel -> a1) += 1
      }
      for ((rel, _) <- cells; a2 <- arg2.asArg2 if unaryToBinary(a2) != rel) {
        pairCounts(rel -> a2) += 1
      }
    }

    val normalizer = rows.size.toDouble + priorCounts.values.sum

    println("Done counting")

    val result = new mutable.HashMap[(String, String), Rule2]()
    for (rel <- singleCounts.keys;
         arg <- singleCountsArgs.keys) {
      val (r1, r2, counts1, counts2) =
        if (rel.compareTo(arg) < 0)
          (rel, arg, singleCounts(rel), singleCountsArgs(arg))
        else
          (arg, rel, singleCountsArgs(arg), singleCounts(rel))
      if (pairCounts(r1, r2) >= 1 || random.nextDouble() < subSample) {
        //        result(rel -> arg) = toRule(rel, arg, pairCounts(rel, arg), singleCounts(rel), singleCountsArgs(arg), normalizer, priorCounts)
        result((r1, r2)) = toRule(r1, r2, pairCounts(r1, r2), counts1, counts2, normalizer, priorCounts)
      }


      //      if (arg == "A2#path#dobj|<-dobj<-replace->prep->in->pobj->|pobj#2") {
      //        val rule = toRule(rel, arg, pairCounts(rel, arg), singleCounts(rel), singleCountsArgs(arg), normalizer, priorCounts)
      //        if (rule.cond1given2 > 0.9)
      //          println(rule)
      //      }

    }
    println("Done!")
    Rules(result.toMap)
  }

}
