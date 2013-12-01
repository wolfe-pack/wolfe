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
    val model = {(unit(x) + unit(y)) dot w} | w -> weights

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
    val term = dsum {for (i <- (0 ~~ n) as "i") yield I(p(i)) + 1.0} | state
    val actual = TermConverter.pushDownConditions(term)
    val pCond = 'p of (0 ~~ (n | state) |-> Bools)
    val expected = dsum {for (i <- (0 ~~ (n | state)) as "i") yield I(pCond(i) | state) + 1.0}
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

  "Pushing down dot products" should "replace dot products of vector sums with double sums of dot products" in {
    val w = 'w of Vectors
    val f1 = vsum(for (i <- (0 ~~ 2) as "i") yield unit(i))
    val f2 = vsum(unit(0), unit(1))
    val term = (f1 + f2) dot w
    val flat = TermConverter.flatten(term, Math.VecAdd)
    val actual = TermConverter.pushDownDotProducts(flat)
    val expected = dsum(dsum(for (i <- (0 ~~ 2) as "i") yield unit(i) dot w), unit(0) dot w, unit(1) dot w)
    actual should be(expected)
  }

  "Grouping lambda abstractions" should "group lambda abstractions over the same domain if they have the same hidden variables" in {
    val p = 'p of Ints |-> Ints
    val f1 = vsum(for (i <- (0 ~~ 2) as "i") yield unit(p(i)))
    val f2 = vsum(for (i <- (0 ~~ 2) as "i") yield unit(p(i) + 1))
    val term = vsum(f1, f2)
    val actual = TermConverter.groupLambdas(term)
    val expected = vsum(for (i <- (0 ~~ 2) as "i") yield vsum(unit(p(i)), unit(p(i) + 1)))
    actual should be(expected)
  }

  "Normalizing linear models" should "result in flat terms with merged lambda abstractions" in {
    val n = 'n of Ints
    val word = 'word of (0 ~~ n |-> Strings)
    val chunk = 'chunk of (0 ~~ n |-> Strings)
    val weights = 'weights of Vectors
    val key = new Index()
    val bias = vsum(for (i <- 0 ~~ n as "i") yield unit(key('bias, chunk(i))))
    val wordChunk = vsum(for (i <- 0 ~~ n as "i") yield unit(key('wordChunk, word(i), chunk(i))))
    val trans = vsum(for (i <- 0 ~~ (n - 1) as "i") yield unit(key('trans, chunk(i), chunk(i + 1))))
    val hard1 = dsum(for (i <- 0 ~~ n) yield log(I(chunk(i) === "O")))
    val hard2 = dsum(for (i <- 0 ~~ n) yield log(I(chunk(i) === "B-NP")))

    val feat = bias + wordChunk + trans
    val model = (feat dot weights) + hard1 + hard2

    val actual = TermConverter.normalizeLinearModel(model, chunk.allAtoms)
    val expected = vsum(
      vsum(for (i <- 0 ~~ (n - 1) as "i") yield unit(key('trans, chunk(i), chunk(i + 1)))),
      vsum(for (i <- 0 ~~ n as "i") yield vsum(unit(key('bias, chunk(i))), unit(key('wordChunk, word(i), chunk(i)))))
    ) dot weights

    println(actual)
    //actual should be (expected)


  }

}
