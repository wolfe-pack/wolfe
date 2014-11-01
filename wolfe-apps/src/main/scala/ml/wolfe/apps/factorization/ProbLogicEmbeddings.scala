package ml.wolfe.apps.factorization

import cc.factorie.la.DenseTensor1
import cc.factorie.model.WeightsSet
import cc.factorie.optimize._
import com.typesafe.config.Config
import ml.wolfe._
import ml.wolfe.Wolfe._
import ml.wolfe.apps.factorization.FactorizationUtil.Row
import ml.wolfe.fg.VectorMsgs
import ml.wolfe.util.{PotentialDebugger, Util}

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
                               average: Boolean = true, forceZero: Boolean = true,
                               usel2dist: Boolean = false, minWhenUsingL2Dist: Boolean = true) {


  def predict(observations: Seq[String], relation: String) = {
    embeddings.get(relation) match {
      case None => 0.0
      case Some(embedding) =>
        val filteredObs = observations.filter(embedding.observationFilter)
        if (!forceZero || filteredObs.size > 0) {
          val normalizer = if (average) filteredObs.size.toDouble else 1.0
          var score = embedding.bias
          if (!usel2dist) for (obs <- filteredObs; obsEmb <- embeddings.get(obs)) {
            score += obsEmb.weight * embedding.scale * (embedding.embedding dot obsEmb.embedding) / normalizer // observations.size
          } else {
            //take the average
            if (minWhenUsingL2Dist) {
              val distances = for (obs <- filteredObs.view; obsEmb <- embeddings.get(obs).view) yield {
                Util.sq(obsEmb.embedding.l2Similarity(embedding.embedding))
              }
              score -= distances.min
            } else {
              val result = new DenseTensor1(embedding.embedding)
              for (obs <- filteredObs; obsEmb <- embeddings.get(obs)) {
                result +=(obsEmb.embedding, -1.0 / normalizer) // observations.size
              }
              score -= result.twoNormSquared
            }
          }
          val result = Util.sig(score)
          result
        } else 0.0
    }
  }
  def predictRow(observation: Row, targets: Seq[String]) = {
    observation.copy(relations = targets.map(r => r -> predict(observation.observedTrue, r)))
  }

  def pairwiseRules(relPairs: Iterable[(String, String)]) = {
    val relations = embeddings.keys.toArray.sorted
    val marginals = (for (r <- relations) yield r -> predict(Seq.empty, r)).toMap
    val result = for ((rel1, rel2) <- relPairs;
                      emb1 = embeddings(rel1);
                      emb2 = embeddings(rel2)) yield {
      val prob1given2 = predict(Seq(rel2), rel1)
      val prob2given1 = predict(Seq(rel1), rel2)
      val prob1 = marginals(rel1)
      val prob2 = marginals(rel2)
      val probs = Map(
        (true, true) -> prob1given2 * prob2, //todo: this may be different to using the other way around
        (true, false) -> (1.0 - prob2given1) * prob1,
        (false, true) -> (1.0 - prob1given2) * prob2,
        (false, false) -> (1 - prob1) * (1 - prob2)
      )
      (rel1, rel2) -> Rule2(
        rel1, rel2, probs,
        trueTrueInconsistency = math.abs(prob1given2 * prob2 - prob2given1 * prob1),
        cond1given2 = prob1given2, cond2given1 = prob2given1)
    }
    result.toMap
  }

}

case class Rules(rules2: Map[(String, String), Rule2], rules1: Map[String, Rule1] = Map.empty) {
  lazy val rel2RuleArg1 = rules2.toSeq.groupBy(_._1._1) withDefaultValue Seq.empty
  lazy val rel2RuleArg2 = rules2.toSeq.groupBy(_._1._2) withDefaultValue Seq.empty

  lazy val relations = rules2.keySet.map(_._1) ++ rules2.keySet.map(_._2)

  def pairwiseRuleCount(rel: String) = rel2RuleArg1(rel).size + rel2RuleArg2(rel).size

  def +(that: Rules): Rules = {
    val result = new mutable.HashMap[(String, String), Rule2]
    for ((pair, r1) <- rules2) {
      that.rules2.get(pair) match {
        case Some(r2) => result(pair) = r1 + r2
        case None => result(pair) = r1
      }
    }
    for ((pair, r2) <- that.rules2) if (!result.contains(pair)) result(pair) = r2
    copy(rules2 = result.toMap)

  }


}


case class Rule2(rel1: String, rel2: String, probs: Map[(Boolean, Boolean), Double], scale: Double = 1,
                 count: Double = 1.0, trueTrueInconsistency: Double = 0.0, cond1given2: Double, cond2given1: Double) {
  def marg1(b1: Boolean) = probs(b1, true) + probs(b1, false)
  def marg2(b2: Boolean) = probs(true, b2) + probs(false, b2)
  def prob2given1(b1: Boolean)(b2: Boolean) = probs(b1, b2) / marg1(b1)
  def prob1given2(b2: Boolean)(b1: Boolean) = probs(b1, b2) / marg2(b2)

  def cooccurCount = count * probs(true, true)
  override def toString =
    s"""$rel1 $rel2  ${ if (trueTrueInconsistency > 0.0) "(" + trueTrueInconsistency.toString + ")" else "" }
      |p(r1|r2) = ${ cond1given2 }
      |p(r2|r1) = ${ cond2given1 }
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
    def newProb(b1:Boolean,b2:Boolean) =  (probs(b1, b2) * count + that.probs(b1, b2) * that.count) / newCount
    val newProbs = Map(
      (true,true) -> newProb(true,true),
      (true,false) -> newProb(true,false),
      (false,true) -> newProb(false,true),
      (false,false) -> newProb(false,false)
    )
    copy(probs = newProbs, count = newCount)
  }

  def klTerm(p1: Double, p2: Double) = if (p1 == 0.0) 0.0 else p1 * math.log(p1 / p2)

  def prob1given2Inc(that: Rule2) = cond1given2 - that.cond1given2

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
    val weighTerms = conf.getBoolean("epl.weigh-terms")
    val unitBall = conf.getBoolean("epl.unit-ball")
    val l2dist = conf.getBoolean("epl.l2-dist")

    val maxMarg = rules.rules2.view.flatMap(t => Iterator(t._2.marg1(true), t._2.marg2(true))).max

    val V = relations.map(r => r -> fg.addVectorNode(k, r)).toMap
    if (unitBall) for (n <- V.values) n.variable.asVector.unitVector = true
    def emptyMap = Map.empty[String, Node] withDefaultValue null
    val colScales = if (regS == Double.PositiveInfinity) emptyMap else relations.map(i => i -> fg.addVectorNode(1)).toMap
    val colBiases = if (regBias == Double.PositiveInfinity) emptyMap else relations.map(i => i -> fg.addVectorNode(1)).toMap
    val colMults = if (regMult == Double.PositiveInfinity) emptyMap else relations.map(i => i -> fg.addVectorNode(1)).toMap

    //initialize
    for (n <- V.values; i <- 0 until k) n.variable.asVector.b(i) = random.nextGaussian() * 1.0
    for (n <- colScales.values) n.variable.asVector.b(0) = scalePrior
    for (n <- colBiases.values) n.variable.asVector.b(0) = biasPrior
    for (n <- colMults.values) n.variable.asVector.b(0) = multPrior

    val numberOfTerms = numRelations * (numRelations - 1) / 2.0
    val objNormalizer = 1.0 / numberOfTerms

    println("Building factor graph")

    var numJointFactors = 0
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
          if (!l2dist) fg.buildFactor(Seq(v1, eta1, s1, m1, v2, eta2, s2, m2))(_ map (_ => new VectorMsgs)) {
            e => new JointPotential(
              e(0), e(1), e(2), e(3),
              e(4), e(5), e(6), e(7),
              rule.prob1given2(true)(true), rule.prob2given1(true)(true),
              rule.marg1(true), rule.marg2(true),
              regW, regBias, regS, regMult,
              biasPrior, scalePrior, multPrior,
              1.0 / relNormalizer1, 1.0 / relNormalizer2,
              if (weighTerms) rule.marg1(true) / maxMarg else 1.0,
              if (weighTerms) rule.marg2(true) / maxMarg else 1.0)
          } else
            fg.buildFactor(Seq(v1, v2, eta1, eta2))(_ map (_ => new VectorMsgs)) {
              e => new L2DistanceBasedPotential(
                e(0), e(1), e(2), e(3),
                rule.prob1given2(true)(true), rule.prob2given1(true)(true),
                1.0,
                regW, regBias, biasPrior,
                1.0 / relNormalizer1, 1.0 / relNormalizer2)
            }
          //if (numJointFactors == 0) PotentialDebugger.checkGradients(factor.potential, debug = true)
          numJointFactors += 1

        case _ =>
      }
    }

    fg.build()
    println(s"Optimizing... with ${ fg.factors.size } terms")

    val maxIterations = conf.getInt("epl.opt-iterations")


//        val step = new AdaGrad(conf.getDouble("epl.ada-rate")) with UnitBallProjection
    val step = new AdaMira(conf.getDouble("epl.ada-rate")) with UnitBallProjection
    //val step = new LBFGS() with UnitBallProjection


    def trainer(weightsSet: WeightsSet) = conf.getString("epl.trainer") match {
      case "batch" => new BatchTrainer(weightsSet, step, maxIterations)
      case "online" => new OnlineTrainer(weightsSet, step, maxIterations)
    }

    GradientBasedOptimizer(fg, trainer(_), step)
    //GradientBasedOptimizer(fg, new BatchTrainer(_, new LBFGS(), maxIterations))

    //allowed observations for each predicate are only the relations we have seen together with the predicate
    val allowed = new mutable.HashMap[String, mutable.HashSet[String]]()
    for ((r1, r2) <- rules.rules2.keys) {
      allowed.getOrElseUpdate(r1, new mutable.HashSet[String]) += r2
      allowed.getOrElseUpdate(r2, new mutable.HashSet[String]) += r1
    }
    //val allPairs = rules.rules2.keySet.flatMap(p => Set(p, p.swap))
    //val allowed = allPairs.groupBy(_._1).mapValues(_.map(_._2))
    val embeddings = relations.map({ rel =>
      rel -> PredicateEmbedding(rel,
        V(rel).variable.asVector.b,
        if (regS == Double.PositiveInfinity) scalePrior else colScales(rel).variable.asVector.b(0),
        if (regBias == Double.PositiveInfinity) biasPrior else colBiases(rel).variable.asVector.b(0),
        if (regMult == Double.PositiveInfinity) multPrior else colMults(rel).variable.asVector.b(0),
        allowed(rel))
    })
    ProbLogicEmbeddings(embeddings.toMap, rules, usel2dist = l2dist)
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
        //todo: more sensible to sort relations here instead adding two versions.
        pairCounts(cells(i)._1 -> cells(j)._1) += 1
        pairCounts(cells(j)._1 -> cells(i)._1) += 1
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
      (rel1, rel2) -> Rule2(rel1, rel2, probs, 1.0, count = normalizer,
        cond1given2 = prob11 / (prob01 + prob11),
        cond2given1 = prob11 / (prob10 + prob11))
    }
    val rules1 = for ((r, c) <- singleCounts) yield r -> Rule1(r, c / normalizer)
    Rules(rules2.toMap, rules1.toMap)
  }
}
