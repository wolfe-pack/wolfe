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
    val model = {(e_(x) + e_(y)) dot w} | w -> weights

    val expected = Inference.exhaustiveArgmax(model)
    //    val actual = Inference.maxProductArgmax(1)(model)
    //    actual.state() should be(expected.state())

    println(expected.state())
    //    println(actual.feats())
    println(expected.feats())
    println(expected.feats()(1))
  }

  "Pushing down conditions" should "move condition terms downward the term tree" in {
    val n = 'n of Ints
    val p = 'p of (0 ~~ n |-> Bools)
    val state = State(Map(n -> 2))
    val term = dsum {for (i <- (0 ~~ n) named "i") yield I(p(i)) + 1.0} | state
    val actual = TermConverter.pushDownConditions(term)
    val pCond = 'p of (0 ~~ (n | state) |-> Bools)
    val expected = dsum {for (i <- (0 ~~ (n | state)) named "i") yield I(pCond(i) | state) + 1.0}
    actual should be(expected)
  }

  "Unrolling images of lambda abstractions" should "create sequences of terms, one for each element in the domain" in {
    val p = 'p of (0 ~~ 3 |-> Bools)
    val term = dsum {for (i <- 0 ~~ 3) yield I(p(i))}
    val expected = dsum(I(p(0)), I(p(1)), I(p(2)))
    val actual = TermConverter.unrollLambdaImages(term)
    actual should be(expected)
  }

  "Flattening terms" should "replace trees of binary function applications with reductions of the function" in {
    val x = 'x of Doubles
    val term = ((x + x) + (x + x)) + dsum(x, x) + x
    val expected = dsum(x, x, x, x, x, x, x)
    val actual = TermConverter.flatten(term, Math.DoubleAdd)
    actual should be(expected)
  }


}
