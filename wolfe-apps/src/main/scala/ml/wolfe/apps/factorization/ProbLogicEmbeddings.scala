package ml.wolfe.apps.factorization

import cc.factorie.la.DenseTensor1
import cc.factorie.optimize.{AdaGrad, BatchTrainer}
import com.typesafe.config.Config
import ml.wolfe._
import ml.wolfe.Wolfe._
import ml.wolfe.apps.factorization.FactorizationUtil.Row
import ml.wolfe.fg.VectorMsgs
import ml.wolfe.util.Util

import scala.collection.mutable

/**
 * @author Sebastian Riedel
 */
case class PredicateEmbedding(rel: String, embedding: FactorieVector, scale: Double, bias: Double)

case class ProbLogicEmbeddings(embeddings: Map[String, PredicateEmbedding], rules: Rules = Rules(Map.empty)) {

  def predict(observations: Seq[String], relation: String) = {
    embeddings.get(relation) match {
      case None => 0.0
      case Some(embedding) =>
        var score = embedding.bias
        for (obs <- observations; obsEmb <- embeddings.get(obs)) {
          score += embedding.scale * (embedding.embedding dot obsEmb.embedding) / observations.size
        }
        val result = Util.sig(score)
        result
    }
  }
  def predictRow(observation:Row, targets:Seq[String]) = {
    observation.copy(relations = targets.map(r => r -> predict(observation.observedTrue,r)))
  }
}

case class Rules(rules: Map[(String, String), Rule])
case class Rule(rel1: String, rel2: String, probs: Map[(Boolean, Boolean), Double], scale: Double = 1)

object RuleInjector {
  def injectImplication(rule:Rule, forward:Boolean = true): Rule = {
    forward match {
      case true =>
        val probs = Map((true, true) -> (rule.probs(true,true) + rule.probs(true,false)) , (true, false) -> 0.0)
        rule.copy(probs = rule.probs ++ probs)
      case false =>
        val probs = Map((true, true) -> (rule.probs(true,true) + rule.probs(false,true)) , (false, true) -> 0.0)
        rule.copy(probs = rule.probs ++ probs)
    }
  }
}

object ProbLogicEmbedder {

  def embed(rules: Rules)(implicit conf: Config): ProbLogicEmbeddings = {

    val relations = rules.rules.values.flatMap(r => Seq(r.rel1, r.rel2)).distinct.sorted.toArray
    val numRelations = relations.size
    val fg = new FactorGraph
    val k = conf.getInt("epl.relation-dim")
    val regW = 0.1
    val regS = 0.1
    val regBias = 0.1
    val V = relations.map(r => r -> fg.addVectorNode(k, r)).toMap
    val colScales = relations.map(i => i -> fg.addVectorNode(1)).toMap
    val colBiases = relations.map(i => i -> fg.addVectorNode(1)).toMap
    val objNormalizer = 1.0 / (numRelations * numRelations)
    val subSample = conf.getDouble("epl.subsample")

    println("Building factor graph")

    for (rel1Index <- 0 until relations.length; rel2Index <- rel1Index + 1 until relations.size) {
      val rel1 = relations(rel1Index)
      val rel2 = relations(rel2Index)

      val v1 = V(rel1)
      val v2 = V(rel2)

      val s1 = colScales(rel1)
      val s2 = colScales(rel2)

      val eta1 = colBiases(rel1)
      val eta2 = colBiases(rel2)

      rules.rules.get((rel1, rel2)) match {
        case Some(rule) =>
          for (b1 <- Seq(true, false); b2 <- Seq(true, false)) {
            val freq_b1b2 = rule.probs(b1, b2) * rule.scale
            val ignore = (!b1 || !b2) && (random.nextDouble() < (1.0 - subSample))
            if (freq_b1b2 > 0.0 && !ignore) {
              val scale = freq_b1b2 * objNormalizer // numCols// (numCols * numCols)
              //learn the left cell
              fg.buildFactor(Seq(v1, v2, s1, eta1))(_ map (_ => new VectorMsgs)) {
                e => new LogPairwiseColWeightedBias(e(0), e(1), e(2), e(3), scale, I(b1), I(b1), I(b2))
              }
              //learn the right cell
              fg.buildFactor(Seq(v2, v1, s2, eta2))(_ map (_ => new VectorMsgs)) {
                e => new LogPairwiseColWeightedBias(e(0), e(1), e(2), e(3), scale, I(b2), I(b2), I(b1))
              }
            }
          }
        case _ =>
      }
    }

    //create L2 regularizers
    for (rel <- relations) {
      val v = V(rel)
      fg.buildFactor(Seq(v))(_ map (_ => new VectorMsgs)) {
        e => new L2Regularizer(e(0), regW * objNormalizer)
      }

      val s = colScales(rel)
      fg.buildFactor(Seq(s))(_ map (_ => new VectorMsgs)) {
        e => new L2RegularizerOffset(e(0), new DenseTensor1(Array(1.0)), regS * objNormalizer)
      }

      val eta = colBiases(rel)
      fg.buildFactor(Seq(eta))(_ map (_ => new VectorMsgs)) {
        e => new L2Regularizer(e(0), regBias * objNormalizer)
      }
    }

    fg.build()
    println("Optimizing...")

    val maxIterations = conf.getInt("epl.opt-iterations")

    //  GradientBasedOptimizer(fg, new OnlineTrainer(_, new AdaGrad(), 100, 100000))
    GradientBasedOptimizer(fg, new BatchTrainer(_, new AdaGrad(), maxIterations))

    val embeddings = relations.map({ rel =>
      rel -> PredicateEmbedding(rel, V(rel).variable.asVector.b, colScales(rel).variable.asVector.b(0), colBiases(rel).variable.asVector.b(0))
    })
    ProbLogicEmbeddings(embeddings.toMap, rules)
  }

}

object RuleLearner {
  def pairwiseRules(rows: Seq[Row]): Rules = {
    val pairCounts = mutable.HashMap[(String, String), Int]() withDefaultValue 0
    val singleCounts = mutable.HashMap[String, Int]() withDefaultValue 0

    for (row <- rows) {
      val cells = row.relations
      for (cell <- cells) singleCounts(cell._1) += 1
      for (i <- 0 until cells.size; j <- i + 1 until cells.size) {
        pairCounts(cells(i)._1 -> cells(j)._1) += 1
      }
    }

    val relations = singleCounts.keys.toArray.sorted
    val normalizer = rows.size.toDouble
    val rules = for (r1 <- 0 until relations.size; r2 <- r1 + 1 until relations.size) yield {
      val rel1 = relations(r1)
      val rel2 = relations(r2)
      val pairCount = pairCounts((rel1, rel2))
      val singleCount1 = singleCounts(rel1)
      val singleCount2 = singleCounts(rel2)
      val prob11 = pairCount / normalizer
      val prob10 = (singleCount1 - pairCounts(rel1, rel2)) / normalizer
      val prob01 = (singleCount2 - pairCounts(rel1, rel2)) / normalizer
      val prob00 = 1.0 - prob11 - prob10 - prob01
      val probs = Map(
        (true, true) -> prob11, (true, false) -> prob10,
        (false, true) -> prob01, (false, false) -> prob00
      )
      (rel1,rel2) -> Rule(rel1,rel2,probs,1.0)
    }
    Rules(rules.toMap)
  }
}
