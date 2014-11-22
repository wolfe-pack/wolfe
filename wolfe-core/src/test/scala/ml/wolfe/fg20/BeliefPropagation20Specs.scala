package ml.wolfe.fg20

import ml.wolfe._

/**
 * @author Sebastian Riedel
 */
class BeliefPropagation20Specs extends WolfeSpec {

  import math._

  val potFunction: Array[Int] => Double = {
    case Array(0, 0) => 1
    case Array(0, 1) => 2
    case Array(1, 0) => -3
    case Array(1, 1) => 0
  }

  val v1            = new DiscVar(Seq(false, true))
  val v2            = new DiscVar(Seq(false, true))
  val scores        = Array(1.0, 2.0, -3.0, 0.0)
  val tablePot      = new TablePotential(Array(v1, v2), scores)
  val simpleProblem = Problem(Seq(tablePot), Seq(v1, v2))

  val tablePot2      = new TablePotential2(Array(v1, v2), scores)
  val simpleProblem2 = Problem(Seq(tablePot2), Seq(v1, v2))

  val xorPot     = new TablePotential(Array(v1, v2), Array(Double.NegativeInfinity, 0, 0, Double.NegativeInfinity))
  val xorProblem = Problem(Seq(xorPot), Seq(v1, v2))

  val xorPot2     = new TablePotential2(Array(v1, v2), Array(Double.NegativeInfinity, 0, 0, Double.NegativeInfinity))
  val xorProblem2 = Problem(Seq(xorPot2), Seq(v1, v2))


  val fixedStats = LinearPotential.stats(Array(2, 2), {
    case Array(i, j) => LinearPotential.singleton(2 * i + j, 1.0)
  })

  def chainProblem(length: Int) = {
    val vars = for (i <- 0 until length) yield new DiscVar(Seq(false, true), "v" + i)
    val pots = for ((v1, v2) <- vars.dropRight(1) zip vars.drop(1)) yield new TablePotential(Array(v1, v2), scores)
    Problem(pots, vars)
  }

  def chainProblem2(length: Int) = {
    val vars = for (i <- 0 until length) yield new DiscVar(Seq(false, true), "v" + i)
    val pots = for ((v1, v2) <- vars.dropRight(1) zip vars.drop(1)) yield new TablePotential2(Array(v1, v2), scores)
    Problem(pots, vars)
  }


  def chainProblemWithFeatures(length: Int) = {
    val vars = for (i <- 0 until length) yield new DiscVar(Seq(false, true), "v" + i)
    val pots = for ((v1, v2) <- vars.dropRight(1) zip vars.drop(1)) yield new LinearPotential(Array(v1, v2), fixedStats)
    Problem(pots, vars)
  }

  def sameVector(v1: FactorieVector, v2: FactorieVector, eps: Double = 0.00001) = {
    v1.activeDomain.forall(i => math.abs(v1(i) - v2(i)) < eps) &&
    v2.activeDomain.forall(i => math.abs(v1(i) - v2(i)) < eps)
  }

  "A BrufeForce algorithm" should {
    "return the exact log partition function" in {
      import scala.math._
      val bf = new BruteForce(simpleProblem)
      val result = bf.inferMarginals()
      val logZ = log(scores.map(exp).sum)
      result.logZ should be(logZ)
    }
    "return the exact MAP value" in {
      val problem = Problem(Seq(tablePot), Seq(v1, v2))
      val bf = new BruteForce(problem)
      val result = bf.inferMAP()
      val max = scores.max
      result.score should be(max)
    }
    "return the exact conditional MAP value in the presence of observations" in {
      val problem = Problem(Seq(tablePot), Seq(v1, v2), observation = State.single(v1, true))
      val bf = new BruteForce(problem)
      val result = bf.inferMAP()
      val max = 0
      result.score should be(max)
    }

    "return the exact conditional logZ value in the presence of observations" in {
      val problem = Problem(Seq(tablePot), Seq(v1, v2), observation = State.single(v1, true))
      val bf = new BruteForce(problem)
      val result = bf.inferMarginals()
      val logZ = log(exp(0) + exp(-3))
      result.logZ should be(logZ)
    }
  }

  "A BrufeForce2 algorithm" should {
    "return the exact log partition function" in {
      import scala.math._
      val bf = new BruteForce2(simpleProblem2)
      val result = bf.inferMarginals()
      val logZ = log(scores.map(exp).sum)
      result.logZ should be(logZ)
    }
    "return the exact MAP value" in {
      val problem = Problem(Seq(tablePot2), Seq(v1, v2))
      val bf = new BruteForce2(problem)
      val result = bf.inferMAP()
      val max = scores.max
      result.score should be(max)
    }
    "return the exact conditional MAP value in the presence of observations" in {
      val problem = Problem(Seq(tablePot2), Seq(v1, v2), observation = State.single(v1, true))
      val bf = new BruteForce2(problem)
      val result = bf.inferMAP()
      val max = 0
      result.score should be(max)
    }

    "return the exact conditional logZ value in the presence of observations" in {
      val problem = Problem(Seq(tablePot2), Seq(v1, v2), observation = State.single(v1, true))
      val bf = new BruteForce2(problem)
      val result = bf.inferMarginals()
      val logZ = log(exp(0) + exp(-3))
      result.logZ should be(logZ)
    }
  }


  "A Max Product algorithm" should {
    "return the exact max-marginals when given a single table potential" in {
      val fg_mp = new MaxProduct(simpleProblem)
      val fg_bf = new BruteForce(simpleProblem)

      val mpResult = fg_mp.inferMAP(1)
      val bfResult = fg_bf.inferMAP()

      mpResult.maxMarginals should equal(bfResult.maxMarginals)
    }
    "choose a valid global max from a factor graph with multiple solutions" in {
      val mp = new MaxProduct(xorProblem)
      val result = mp.inferMAP()
      result.state(v1) should not be result.state(v2)
    }

    "return the exact max-marginals given a chain" in {
      val chain = chainProblem(5)

      val mp = new MaxProduct(chain).inferMAP()
      val bf = new BruteForce(chain).inferMAP()

      mp.maxMarginals should equal(bf.maxMarginals)

    }

    "return feature vectors of argmax state" in {
      val chain = chainProblemWithFeatures(5)
      val weights = LinearPotential.dense(4, 0 -> 1.0, 1 -> 2.0, 2 -> -3, 3 -> 0)

      val mp = new MaxProduct(chain).inferMAP(weights = weights)
      val bf = new BruteForce(chain).inferMAP(weights)

      sameVector(mp.gradient, bf.gradient) should be(true)
    }
  }

  "A Max Product2 algorithm" should {
    "return the exact max-marginals when given a single table potential" in {
      val fg_mp = new MaxProduct2(simpleProblem2)
      val fg_bf = new BruteForce2(simpleProblem2)

      val mpResult = fg_mp.inferMAP(1)
      val bfResult = fg_bf.inferMAP()

      mpResult.maxMarginals should equal(bfResult.maxMarginals)
    }
    "choose a valid global max from a factor graph with multiple solutions" in {
      val mp = new MaxProduct2(xorProblem2)
      val result = mp.inferMAP()
      result.state(v1) should not be result.state(v2)
    }

    "return the exact max-marginals given a chain" in {
      val chain = chainProblem2(5)

      val mp = new MaxProduct2(chain).inferMAP()
      val bf = new BruteForce2(chain).inferMAP()

      mp.maxMarginals should equal(bf.maxMarginals)

    }

    "return feature vectors of argmax state" in {
      val chain = chainProblemWithFeatures(5)
      val weights = LinearPotential.dense(4, 0 -> 1.0, 1 -> 2.0, 2 -> -3, 3 -> 0)

      val mp = new MaxProduct(chain).inferMAP(weights = weights)
      val bf = new BruteForce(chain).inferMAP(weights)

      sameVector(mp.gradient, bf.gradient) should be(true)
    }
  }


  "A Sum Product algorithm" should {
    "return the exact marginals when given a single table potential" in {
      val fg_mp = new SumProduct(simpleProblem)
      val fg_bf = new BruteForce(simpleProblem)

      val mpResult = fg_mp.inferMarginals(1)
      val bfResult = fg_bf.inferMarginals()

      mpResult.marginals should equal(bfResult.marginals)
    }

    "return the exact marginals given a chain" in {
      val chain = chainProblem(5)

      val mp = new SumProduct(chain).inferMarginals()
      val bf = new BruteForce(chain).inferMarginals()

      mp.marginals should equal(bf.marginals)

    }

    "return expected feature vectors as gradient" in {
      val chain = chainProblemWithFeatures(5)
      val weights = LinearPotential.dense(4, 0 -> 1.0, 1 -> 2.0, 2 -> -3, 3 -> 0)

      val mp = new SumProduct(chain).inferMarginals(weights = weights)
      val bf = new BruteForce(chain).inferMarginals(weights)

      sameVector(mp.gradient, bf.gradient) should be(true)
    }


  }


}
