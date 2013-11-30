package scalapplcodefest

import org.scalatest.{Matchers, FlatSpec}

/**
 * Set of specs.
 * @author Sebastian Riedel
 */
class Specs extends FlatSpec with Matchers {

  import TermImplicits._

  "An exhaustive argmaxer" should "find the maximizing assignment of a term" in {
    val x = 'x of Bools
    val y = 'y of Bools
    val model = I(x && !y)
    val argmax = Inference.exhaustiveArgmax(model).state()
    argmax(x) should be(true)
    argmax(y) should be(false)
  }

  "A MaxProduct argmaxer" should "find the maximizing assignment of a chain" in {
    val x = 'x of Bools
    val y = 'y of Bools
    val z = 'z of Bools
    val model = I(x |=> y) + I(y |=> z) + I(x)
    val expected = Inference.exhaustiveArgmax(model).state()
    val actual = Inference.maxProductArgmax(1)(model).state()
    actual should be(expected)
  }

  "A MaxProduct argmaxer" should "provide argmax feature vectors for linear models" in {
    val x = 'x of 0 ~~ 2
    val y = 'y of 0 ~~ 2
    val w = 'w of Vectors
    val index = new Index
    val weights = index.createDenseVector(Seq(0) -> -1.0, Seq(1) -> 1.0)()
    val model = {(e_(x) + e_(y)) dot w }| w -> weights

    val expected = Inference.exhaustiveArgmax(model)
//    val actual = Inference.maxProductArgmax(1)(model)
//    actual.state() should be(expected.state())

    println(expected.state())
//    println(actual.feats())
    println(expected.feats())
    println(expected.feats()(1))
  }


}
