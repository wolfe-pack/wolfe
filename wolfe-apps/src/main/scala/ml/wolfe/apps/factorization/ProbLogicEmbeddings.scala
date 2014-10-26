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
case class PredicateEmbedding(rel: String, embedding: FactorieVector, scale: Double, bias: Double, weight: Double = 1.0)

case class ProbLogicEmbeddings(embeddings: Map[String, PredicateEmbedding],
                               rules: Rules = Rules(Map.empty,Map.empty), average: Boolean = false) {

  def predict(observations: Seq[String], relation: String) = {
    val normalizer = if (average) observations.size.toDouble else 1.0
    embeddings.get(relation) match {
      case None => 0.0
      case Some(embedding) =>
        var score = embedding.bias
        for (obs <- observations; obsEmb <- embeddings.get(obs)) {
          score += obsEmb.weight * embedding.scale * (embedding.embedding dot obsEmb.embedding) / normalizer // observations.size
        }
        val result = Util.sig(score)
        result
    }
  }
  def predictRow(observation: Row, targets: Seq[String]) = {
    observation.copy(relations = targets.map(r => r -> predict(observation.observedTrue, r)))
  }
}

case class Rules(rules2: Map[(String, String), Rule2], rules1:Map[String,Rule1])
case class Rule2(rel1: String, rel2: String, probs: Map[(Boolean, Boolean), Double], scale: Double = 1) {
  def marg1(b1: Boolean) = probs(b1, true) + probs(b1, false)
  def marg2(b2: Boolean) = probs(true, b2) + probs(false, b2)
  def cond12(b1: Boolean)(b2: Boolean) = probs(b1, b2) / marg1(b1)
  def cond21(b2: Boolean)(b1: Boolean) = probs(b1, b2) / marg1(b2)
}
case class Rule1(rel:String, prob:Double)

object RuleInjector {
  def injectImplication(rule: Rule2, forward: Boolean = true): Rule2 = {
    forward match {
      case true =>
        val probs = Map((true, true) -> (rule.probs(true, true) + rule.probs(true, false)), (true, false) -> 0.0)
        rule.copy(probs = rule.probs ++ probs)
      case false =>
        val probs = Map((true, true) -> (rule.probs(true, true) + rule.probs(false, true)), (false, true) -> 0.0)
        rule.copy(probs = rule.probs ++ probs)
    }
  }
}

object ProbLogicEmbedder {

  def embed(rules: Rules)(implicit conf: Config): ProbLogicEmbeddings = {

    val relations = rules.rules2.values.flatMap(r => Seq(r.rel1, r.rel2)).distinct.sorted.toArray
    val numRelations = relations.size
    val fg = new FactorGraph
    val k = conf.getInt("epl.relation-dim")
    val regW = conf.getDouble("epl.reg-embed")
    val regS = conf.getDouble("epl.reg-scale")
    val regBias = conf.getDouble("epl.reg-bias")
    val regMult = conf.getDouble("epl.reg-mult")
    val doNormB = conf.getBoolean("epl.norm-b")
    val scalePrior = conf.getDouble("epl.scale-prior")
    val biasPrior = conf.getDouble("epl.bias-prior")
    val multPrior = conf.getDouble("epl.mult-prior")


    val V = relations.map(r => r -> fg.addVectorNode(k, r)).toMap
    val colScales = relations.map(i => i -> fg.addVectorNode(1)).toMap
    val colBiases = relations.map(i => i -> fg.addVectorNode(1)).toMap
    val colMults = relations.map(i => i -> fg.addVectorNode(1)).toMap
    val objNormalizer = 1.0 / (numRelations * (numRelations - 1) / 2.0)
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

      val m1 = colMults(rel1)
      val m2 = colMults(rel2)

      rules.rules2.get((rel1, rel2)) match {
        case Some(rule) =>
          for (b <- Seq(true, false)) {
            val cond = rule.cond21(true)(b)
            if (cond > 0.0) {
              val scale = cond * objNormalizer // numCols// (numCols * numCols)
              fg.buildFactor(Seq(v1, v2, s1, eta1, m1, m2))(_ map (_ => new VectorMsgs)) {
                e => new LogPairwiseWeightedScaleBias(e(0), e(1), e(2), e(3), e(4), e(5), scale, I(b), 0, 1)
              }
            }
          }
          for (b <- Seq(true, false)) {
            val cond = rule.cond12(true)(b)
            if (cond > 0.0) {
              val scale = cond * objNormalizer // numCols// (numCols * numCols)
              fg.buildFactor(Seq(v2, v1, s2, eta2, m2, m1))(_ map (_ => new VectorMsgs)) {
                e => new LogPairwiseWeightedScaleBias(e(0), e(1), e(2), e(3), e(4), e(5), scale, I(b), 0, 1)
              }
            }
          }


//          for (b1 <- Seq(true, false); b2 <- Seq(true, false)) {
//            val freq_b1b2 = rule.probs(b1, b2) * rule.scale
//            val ignore = (!b1 || !b2) && (random.nextDouble() < (1.0 - subSample))
//            val bNorm = if (doNormB && (b1 || b2)) I(b1) + I(b2) else 1.0
//            if (freq_b1b2 > 0.0 && !ignore) {
//              val scale = freq_b1b2 * objNormalizer // numCols// (numCols * numCols)
//              //learn the left cell
//              fg.buildFactor(Seq(v1, v2, s1, eta1, m1, m2))(_ map (_ => new VectorMsgs)) {
//                e => new LogPairwiseWeightedScaleBias(e(0), e(1), e(2), e(3), e(4), e(5), scale, I(b1), I(b1) / bNorm, I(b2) / bNorm)
//              }
//              //learn the right cell
//              fg.buildFactor(Seq(v2, v1, s2, eta2, m2, m1))(_ map (_ => new VectorMsgs)) {
//                e => new LogPairwiseWeightedScaleBias(e(0), e(1), e(2), e(3), e(4), e(5), scale, I(b2), I(b2) / bNorm, I(b1) / bNorm)
//              }
//            }
//          }
        case _ =>
      }
    }

    //create L2 regularizers
    for (rel <- relations) {

      val v = V(rel)
      val s = colScales(rel)
      val eta = colBiases(rel)
      val mult = colMults(rel)

      rules.rules1.get(rel).foreach(rule => {
        fg.buildFactor(Seq(eta))(_ map (_ => new VectorMsgs)) {
          e => new SingleColumnBias(e(0),objNormalizer,rule.prob)
        }
      })

      //likelihood of marginals
      fg.buildFactor(Seq(v))(_ map (_ => new VectorMsgs)) {
        e => new L2Regularizer(e(0), regW * objNormalizer)
      }

      fg.buildFactor(Seq(s))(_ map (_ => new VectorMsgs)) {
        e => new L2RegularizerOffset(e(0), new DenseTensor1(Array(scalePrior)), regS * objNormalizer)
      }

      fg.buildFactor(Seq(eta))(_ map (_ => new VectorMsgs)) {
        e => new L2RegularizerOffset(e(0), new DenseTensor1(Array(biasPrior)), regBias * objNormalizer)
      }

      fg.buildFactor(Seq(mult))(_ map (_ => new VectorMsgs)) {
        e => new L2RegularizerOffset(e(0), new DenseTensor1(Array(multPrior)), regMult * objNormalizer)
      }

    }

    fg.build()
    println("Optimizing...")

    val maxIterations = conf.getInt("epl.opt-iterations")

    //  GradientBasedOptimizer(fg, new OnlineTrainer(_, new AdaGrad(), 100, 100000))
    GradientBasedOptimizer(fg, new BatchTrainer(_, new AdaGrad(), maxIterations))

    val embeddings = relations.map({ rel =>
      rel -> PredicateEmbedding(rel,
        V(rel).variable.asVector.b,
        colScales(rel).variable.asVector.b(0),
        colBiases(rel).variable.asVector.b(0),
        colMults(rel).variable.asVector.b(0))
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
    val rules2 = for (r1 <- 0 until relations.size; r2 <- r1 + 1 until relations.size) yield {
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
      (rel1, rel2) -> Rule2(rel1, rel2, probs, 1.0)
    }
    val rules1 = for ((r,c) <- singleCounts) yield r -> Rule1(r, c / normalizer)
    Rules(rules2.toMap, rules1.toMap)
  }
}
