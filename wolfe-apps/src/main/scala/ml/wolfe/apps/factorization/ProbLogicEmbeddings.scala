package ml.wolfe.apps.factorization

import cc.factorie.la.DenseTensor1
import cc.factorie.model.WeightsSet
import cc.factorie.optimize.{OnlineTrainer, LBFGS, AdaGrad, BatchTrainer}
import com.typesafe.config.Config
import ml.wolfe._
import ml.wolfe.Wolfe._
import ml.wolfe.apps.factorization.FactorizationUtil.Row
import ml.wolfe.fg.VectorMsgs
import ml.wolfe.util.Util

import scala.collection.mutable
import scala.util.Random

/**
 * @author Sebastian Riedel
 */
case class PredicateEmbedding(rel: String, embedding: FactorieVector,
                              scale: Double, bias: Double, weight: Double = 1.0,
                              observationFilter: String => Boolean = _ => true)

case class ProbLogicEmbeddings(embeddings: Map[String, PredicateEmbedding],
                               rules: Rules = Rules(Map.empty, Map.empty),
                               average: Boolean = true) {

  def predict(observations: Seq[String], relation: String) = {
    val normalizer = if (average) observations.size.toDouble else 1.0
    embeddings.get(relation) match {
      case None => 0.0
      case Some(embedding) =>
        var score = embedding.bias
        for (obs <- observations; if embedding.observationFilter(obs); obsEmb <- embeddings.get(obs)) {
          score += obsEmb.weight * embedding.scale * (embedding.embedding dot obsEmb.embedding) / normalizer // observations.size
        }
        val result = Util.sig(score)
        result
    }
  }
  def predictRow(observation: Row, targets: Seq[String]) = {
    observation.copy(relations = targets.map(r => r -> predict(observation.observedTrue, r)))
  }

  lazy val pairwiseRules = {
    val relations = embeddings.keys.toArray.sorted
    val result = for (r1 <- 0 until relations.size; r2 <- r1 + 1 until relations.size) yield {
      val rel1 = relations(r1)
      val rel2 = relations(r2)
      val emb1 = embeddings(rel1)
      val emb2 = embeddings(rel2)
      val prob1given2 = predict(Seq(rel2), rel1)
      val prob2given1 = predict(Seq(rel1), rel2)
      val prob1 = predict(Seq.empty, rel1)
      val prob2 = predict(Seq.empty, rel2)
      val probs = Map(
        (true, true) -> prob1given2 * prob2, //todo: this may be different to using the other way around
        (true, false) -> (1.0 - prob2given1) * prob1,
        (false, true) -> (1.0 - prob1given2) * prob2,
        (false, false) -> (1 - prob1) * (1 - prob2)
      )
      (rel1, rel2) -> Rule2(rel1, rel2, probs, trueTrueInconsistency = math.abs(prob1given2 * prob2 - prob2given1 * prob1))
    }
    result.toMap
  }

}

case class Rules(rules2: Map[(String, String), Rule2], rules1: Map[String, Rule1]) {
  lazy val rel2RuleArg1 = rules2.toSeq.groupBy(_._1._1) withDefaultValue Seq.empty
  lazy val rel2RuleArg2 = rules2.toSeq.groupBy(_._1._2) withDefaultValue Seq.empty

  def pairwiseRuleCount(rel: String) = rel2RuleArg1(rel).size + rel2RuleArg2(rel).size

  def +(that: Rules): Rules = {
    val keys = rules2.keySet ++ that.rules2.keySet
    val result = for (k <- keys) yield (rules2.get(k), that.rules2.get(k)) match {
      case (Some(r1), Some(r2)) => k -> (r1 + r2)
      case (Some(r1), _) => k -> r1
      case (_, Some(r2)) => k -> r2
      case _ => sys.error("can't happen")
    }
    copy(rules2 = result.toMap)
  }


}
case class Rule2(rel1: String, rel2: String, probs: Map[(Boolean, Boolean), Double], scale: Double = 1,
                 count: Double = 1.0, trueTrueInconsistency: Double = 0.0) {
  def marg1(b1: Boolean) = probs(b1, true) + probs(b1, false)
  def marg2(b2: Boolean) = probs(true, b2) + probs(false, b2)
  def prob2given1(b1: Boolean)(b2: Boolean) = probs(b1, b2) / marg1(b1)
  def prob1given2(b2: Boolean)(b1: Boolean) = probs(b1, b2) / marg2(b2)
  override def toString =
    s"""$rel1 $rel2  ${ if (trueTrueInconsistency > 0.0) "(" + trueTrueInconsistency.toString + ")" else "" }
      |p(r1|r2) = ${ prob1given2(true)(true) }
      |p(r2|r1) = ${ prob2given1(true)(true) }
      |p(r1)    = ${ marg1(true) }
      |p(r2)    = ${ marg2(true) }
    """.stripMargin

  lazy val mutualInformation = {
    probs(true, true) * math.log(probs(true, true) / (marg1(true) * marg2(true))) +
    probs(true, false) * math.log(probs(true, false) / (marg1(true) * marg2(false))) +
    probs(false, true) * math.log(probs(false, true) / (marg1(false) * marg2(true))) +
    probs(false, false) * math.log(probs(false, false) / (marg1(false) * marg2(false)))
  }

  def +(that: Rule2) = {
    val newCount = count + that.count
    val newProbs = for (b1 <- List(false, true); b2 <- List(false, true)) yield
      (b1, b2) -> (probs(b1, b2) * count + that.probs(b1, b2) * that.count) / newCount
    copy(probs = newProbs.toMap, count = newCount)
  }

  def klTerm(p1: Double, p2: Double) = if (p1 == 0.0) 0.0 else p1 * math.log(p1 / p2)

  def prob1given2Inc(that: Rule2) = prob1given2(true)(true) - that.prob1given2(true)(true)

  def condKL(that: Rule2) = {
    klTerm(prob1given2(true)(true), that.prob1given2(true)(true)) +
    klTerm(prob1given2(true)(false), that.prob1given2(true)(false)) +
    klTerm(prob2given1(true)(true), that.prob2given1(true)(true)) +
    klTerm(prob2given1(true)(false), that.prob2given1(true)(false))
  }

  def kl(that: Rule2) = {
    klTerm(probs(true, true), that.probs(true, true)) +
    klTerm(probs(true, false), that.probs(true, false)) +
    klTerm(probs(false, true), that.probs(false, true)) +
    klTerm(probs(false, true), that.probs(false, false))
  }

}
case class Rule1(rel: String, prob: Double)

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

    import FactorGraph.Node

    val random = new Random(0)
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
    def emptyMap = Map.empty[String, Node] withDefaultValue null
    val colScales = if (regS == Double.PositiveInfinity) emptyMap else relations.map(i => i -> fg.addVectorNode(1)).toMap
    val colBiases = if (regBias == Double.PositiveInfinity) emptyMap else relations.map(i => i -> fg.addVectorNode(1)).toMap
    val colMults = if (regMult == Double.PositiveInfinity) emptyMap else relations.map(i => i -> fg.addVectorNode(1)).toMap

    //initialize
    for (n <- V.values; i <- 0 until k) n.variable.asVector.b(i) = random.nextGaussian() * 0.01
    for (n <- colScales.values) n.variable.asVector.b(0) = scalePrior
    for (n <- colBiases.values) n.variable.asVector.b(0) = biasPrior
    for (n <- colMults.values) n.variable.asVector.b(0) = multPrior

    val numberOfTerms = numRelations * (numRelations - 1) / 2.0
    val objNormalizer = 1.0 / numberOfTerms

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

      val relNormalizer1 = rules.pairwiseRuleCount(rel1)
      val relNormalizer2 = rules.pairwiseRuleCount(rel2)

      rules.rules2.get((rel1, rel2)) match {
        case Some(rule) =>
          fg.buildFactor(Seq(v1, eta1, s1, m1, v2, eta2, s2, m2))(_ map (_ => new VectorMsgs)) {
            e => new JointPotential(
              e(0), e(1), e(2), e(3),
              e(4), e(5), e(6), e(7),
              rule.prob1given2(true)(true), rule.prob2given1(true)(true),
              rule.marg1(true), rule.marg2(true),
              regW, regBias, regS, regMult,
              biasPrior, scalePrior, multPrior,
              1.0 / relNormalizer1, 1.0 / relNormalizer2)
          }
        case _ =>
      }
    }

    fg.build()
    println(s"Optimizing... with ${ fg.factors.size } terms")

    val maxIterations = conf.getInt("epl.opt-iterations")

    def trainer(weightsSet: WeightsSet) = conf.getString("epl.trainer") match {
      case "batch" => new BatchTrainer(weightsSet, new AdaGrad(conf.getDouble("epl.ada-rate")), maxIterations)
      case "online" => new OnlineTrainer(weightsSet, new AdaGrad(conf.getDouble("epl.ada-rate")), maxIterations)
    }

    GradientBasedOptimizer(fg, trainer(_))
    //GradientBasedOptimizer(fg, new BatchTrainer(_, new LBFGS(), maxIterations))

    //allowed observations for each predicate are only the relations we have seen together with the predicate
    val allPairs = rules.rules2.keySet.flatMap(p => Set(p, p.swap))
    val allowed = allPairs.groupBy(_._1).mapValues(_.map(_._2))
    val embeddings = relations.map({ rel =>
      rel -> PredicateEmbedding(rel,
        V(rel).variable.asVector.b,
        if (regS == Double.PositiveInfinity) scalePrior else colScales(rel).variable.asVector.b(0),
        if (regBias == Double.PositiveInfinity) biasPrior else colBiases(rel).variable.asVector.b(0),
        if (regMult == Double.PositiveInfinity) multPrior else colMults(rel).variable.asVector.b(0),
        allowed(rel))
    })
    ProbLogicEmbeddings(embeddings.toMap, rules)
  }

}

object OldProbLogicEmbedder {

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
            val cond = rule.prob1given2(true)(b)
            if (cond > 0.0) {
              val scale = cond * objNormalizer // numCols// (numCols * numCols)
              fg.buildFactor(Seq(v1, v2, s1, eta1, m1, m2))(_ map (_ => new VectorMsgs)) {
                e => new LogPairwiseWeightedScaleBias(e(0), e(1), e(2), e(3), e(4), e(5), scale, I(b), 0, 1)
              }
            }
          }
          for (b <- Seq(true, false)) {
            val cond = rule.prob2given1(true)(b)
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
          e => new SingleColumnBias(e(0), objNormalizer, rule.prob)
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

    //GradientBasedOptimizer(fg, new OnlineTrainer(_, new AdaGrad(), 10, 100))
    GradientBasedOptimizer(fg, new BatchTrainer(_, new AdaGrad(conf.getDouble("epl.ada-rate")), maxIterations))
    //GradientBasedOptimizer(fg, new BatchTrainer(_, new LBFGS(), maxIterations))


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
  def learn(rows: Seq[Row]): Rules = {
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
      (rel1, rel2) -> Rule2(rel1, rel2, probs, 1.0, count = normalizer)
    }
    val rules1 = for ((r, c) <- singleCounts) yield r -> Rule1(r, c / normalizer)
    Rules(rules2.toMap, rules1.toMap)
  }
}
