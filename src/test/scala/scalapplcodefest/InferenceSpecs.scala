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

  def e(d:Double) = StrictMath.exp(d)

  implicit val random = new Random(0)

  def singleTermMarginals(newInferencer: => (LambdaAbstraction[Boolean, Double] => State)) {
    "find marginals of a single linear term" in {
      val x = 'x of bools
      val posWeight = random.nextGaussian()
      val negWeight = random.nextGaussian()
      val t = fun(Map(true -> posWeight, false -> negWeight), Bools, Doubles)(x)
      val beliefs = newInferencer(lam(x, t))
      val margs = beliefs(Belief(x)).asInstanceOf[Fun[Boolean, Double]]
      margs(true) should be(e(posWeight)/(e(posWeight) + e(negWeight)) +- 0.02)
      margs(false) should be(e(negWeight)/(e(posWeight) + e(negWeight)) +- 0.02)
    }
  }

  def gibbs[T]: LambdaAbstraction[T, Double] => State = l => GibbsSampling(l).infer(State.empty, 1000, 10, 1)
  //def bruteForce[T] = max(_: LambdaAbstraction[T, Double]).byBruteForce

  "Gibbs Sampling" should {
    behave like singleTermMarginals(gibbs)
  }
}
