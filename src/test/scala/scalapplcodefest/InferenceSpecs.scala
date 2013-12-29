package scalapplcodefest

import scalapplcodefest.term._
import org.scalatest.{Matchers, WordSpec}
import scalapplcodefest.value.{Fun, Bools, Doubles}
import scalapplcodefest.term.LambdaAbstraction
import scala.util.Random

/**
 * @author sameer
 */
class InferenceSpecs extends WordSpec with Matchers {

  import TermDSL._

  def e(d: Double) = StrictMath.exp(d)
  implicit def bool2int(b: Boolean) = if (b) 1 else 0

  implicit val random = new Random(0)

  def singleTermMarginals(newInferencer: => (LambdaAbstraction[Boolean, Double] => State)) {
    "find marginals of a single linear term" in {
      val x = 'x of bools
      val posWeight = random.nextGaussian()
      val negWeight = random.nextGaussian()
      val t = fun(Map(true -> posWeight, false -> negWeight), Bools, Doubles)(x)
      val beliefs = newInferencer(lam(x, t))
      val margs = beliefs(Belief(x)).asInstanceOf[Fun[Boolean, Double]]
      margs(true) should be(e(posWeight) / (e(posWeight) + e(negWeight)) +- 0.02)
      margs(false) should be(e(negWeight) / (e(posWeight) + e(negWeight)) +- 0.02)
    }
  }

  def twoVariableMarginals(newInferencer: => (LambdaAbstraction[(Boolean, Boolean), Double] => State)) {
    "find marginals of two variables" in {
      val x = 'x of bools
      val y = 'y of bools
      val xb = fun(Map(true -> random.nextGaussian(), false -> random.nextGaussian()), Bools, Doubles)(x)
      val yb = fun(Map(true -> random.nextGaussian(), false -> random.nextGaussian()), Bools, Doubles)(y)
      val map = Map(
        (true, true) -> random.nextGaussian(),
        (true, false) -> random.nextGaussian(),
        (false, true) -> random.nextGaussian(),
        (false, false) -> random.nextGaussian())
      val xy = fun(map, map.keySet)((x, y))
      val model = xb + yb + xy
      // true marginals
      var Z = 0.0
      val tmargsX = Array.ofDim[Double](2)
      val tmargsY = Array.ofDim[Double](2)
      for (xv <- x.domain.value()) {
        for (yv <- y.domain.value()) {
          val s = State(Map(x -> xv, y -> yv))
          val score = model.value(s)
          val escore = e(score)
          Z += escore
          tmargsX(xv) += escore
          tmargsY(yv) += escore
        }
      }

      val beliefs = newInferencer(lam(sig(x, y), model))
      val margsX = beliefs(Belief(x)).asInstanceOf[Fun[Boolean, Double]]
      for (xv <- x.domain.value()) {
        println("margsX(%s) -> %f\t(%f)" format(xv, margsX(xv), tmargsX(xv) / Z))
        margsX(xv) should be(tmargsX(xv) / Z +- 0.02)
      }
      val margsY = beliefs(Belief(y)).asInstanceOf[Fun[Boolean, Double]]
      for (yv <- y.domain.value()) {
        println("margsY(%s) -> %f\t(%f)" format(yv, margsY(yv), tmargsY(yv) / Z))
        margsY(yv) should be(tmargsY(yv) / Z +- 0.02)
      }
    }
  }

  def threeVariableMarginals(loop: Boolean, newInferencer: => (LambdaAbstraction[(Boolean, Boolean, Boolean), Double] => State)) {
    "find marginals of a three variable model" in {
      val x = 'x of bools
      val y = 'y of bools
      val z = 'z of bools
      val xb = fun(Map(true -> random.nextGaussian(), false -> random.nextGaussian()), Bools, Doubles)(x)
      val yb = fun(Map(true -> random.nextGaussian(), false -> random.nextGaussian()), Bools, Doubles)(y)
      val zb = fun(Map(true -> random.nextGaussian(), false -> random.nextGaussian()), Bools, Doubles)(z)
      val xyMap = Map(
        (true, true) -> random.nextGaussian(),
        (true, false) -> random.nextGaussian(),
        (false, true) -> random.nextGaussian(),
        (false, false) -> random.nextGaussian())
      val xy = fun(xyMap, xyMap.keySet)((x, y))
      val yzMap = Map(
        (true, true) -> random.nextGaussian(),
        (true, false) -> random.nextGaussian(),
        (false, true) -> random.nextGaussian(),
        (false, false) -> random.nextGaussian())
      val yz = fun(yzMap, yzMap.keySet)((y, z))
      val xzMap = Map(
        (true, true) -> random.nextGaussian(),
        (true, false) -> random.nextGaussian(),
        (false, true) -> random.nextGaussian(),
        (false, false) -> random.nextGaussian())
      val xz = fun(xzMap, xzMap.keySet)((x, z))
      val chain = xb + yb + zb + xy + yz
      val model = if (loop) chain + xz else chain
      // true marginals
      var Z = 0.0
      val tmargsX = Array.ofDim[Double](2)
      val tmargsY = Array.ofDim[Double](2)
      val tmargsZ = Array.ofDim[Double](2)
      for (xv <- x.domain.value()) {
        for (yv <- y.domain.value()) {
          for (zv <- z.domain.value()) {
            val s = State(Map(x -> xv, y -> yv, z -> zv))
            val score = model.value(s)
            val escore = e(score)
            Z += escore
            tmargsX(xv) += escore
            tmargsY(yv) += escore
            tmargsZ(zv) += escore
          }
        }
      }

      val beliefs = newInferencer(lam(sig(x, y, z), model))
      val margsX = beliefs(Belief(x)).asInstanceOf[Fun[Boolean, Double]]
      for (xv <- x.domain.value()) {
        println("margsX(%s) -> %f\t(%f)" format(xv, margsX(xv), tmargsX(xv) / Z))
        margsX(xv) should be(tmargsX(xv) / Z +- 0.01)
      }
      val margsY = beliefs(Belief(y)).asInstanceOf[Fun[Boolean, Double]]
      for (yv <- y.domain.value()) {
        println("margsY(%s) -> %f\t(%f)" format(yv, margsY(yv), tmargsY(yv) / Z))
        margsY(yv) should be(tmargsY(yv) / Z +- 0.01)
      }
      val margsZ = beliefs(Belief(z)).asInstanceOf[Fun[Boolean, Double]]
      for (zv <- z.domain.value()) {
        println("margsZ(%s) -> %f\t(%f)" format(zv, margsZ(zv), tmargsZ(zv) / Z))
        margsZ(zv) should be(tmargsZ(zv) / Z +- 0.01)
      }
    }
  }

  def gibbs[T]: LambdaAbstraction[T, Double] => State = l => GibbsSampling(l).infer(State.empty, 5000, 100, 100)
  //def bruteForce[T] = max(_: LambdaAbstraction[T, Double]).byBruteForce

  "Gibbs Sampling (single)" should {
    behave like singleTermMarginals(gibbs)
  }

  "Gibbs Sampling (double)" should {
    behave like twoVariableMarginals(gibbs)
  }

  "Gibbs Sampling (chain3)" should {
    behave like threeVariableMarginals(false, gibbs)
  }

  "Gibbs Sampling (loop3)" should {
    behave like threeVariableMarginals(true, gibbs)
  }
}
