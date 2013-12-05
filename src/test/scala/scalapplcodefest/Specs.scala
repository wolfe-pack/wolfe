package scalapplcodefest

import org.scalatest.{Matchers, FlatSpec}
import org.scalautils._

/**
 * Set of specs.
 * @author Sebastian Riedel
 */
class Specs extends FlatSpec with Matchers {

  import TermImplicits._
  import CustomEqualities._

  "A variable" should "evaluate to the value it is assigned to the state" in {
    val x = 'x of Ints
    x.eval(state(x -> 0)) should be (Good(0))
  }

  it should "evaluate to an Undefined object if no value is assigned to it" in {
    val x = 'x of Ints
    x.eval(State.empty) should be(Bad(VariableUndefined(x, State.empty)))
  }

  it should "evaluate to an Undefined object if the assigned value is outside the domain" in {
    val x = 'x of 0 ~~ 2
    x.eval(state(x -> 3)) should be (Bad(VariableUndefined(x, state(x -> 3))))
  }

  "Applications of functions outside of domain" should "evaluate to a Undefined object" in {
    val f = for (i <- 0 ~~ 2) yield i
    val app = f(3)
    app.eval(State.empty) should be(Bad(FunctionNotDefinedAt(app, State.empty)))
  }

  "A FunApp of a predicate" should "return no free variables if conditioned to have all variables set" in {
    val p = 'p of Bools |-> Bools
    val term = p(true) | p.atom(true) -> true
    term.variables should be(Set.empty)
  }

  "A lambda abstraction with tuple arguments" should "evaluate to a function with tuple arguments" in {
    val f = for ((x,y) <- C(Bools,Bools)) yield  x || y
    f.value()(false,false) should be (false)
    f.value()(false,true) should be (true)
  }

  "A map on a set" should "map each element in the set" in {
    val n = 'n of Ints
    val s = 0 ~~ n
    val f = for (i <- Constant(Ints)) yield i + 1
    val m = s mappedBy f
    m.value(n -> 2) should be (Set(1,2))
  }

  "A filter on a set" should "filter each element in the set" in {
    val n = 'n of Ints
    val s = 0 ~~ n
    val f = for (i <- Constant(Ints)) yield i === 1
    val m = s filteredBy f
    m.value(n -> 2) should be (Set(1))
  }

  "A constant function term" should "be acting like the function when doing function application" in {
    val f = funTerm[String,Int]({case x => x.length})
    f.value()("123") should be (3)
  }

  "A partial function term" should "provide a definedAt function" in {
    val f = for (i <- 0 ~~ 2) yield 2 / i
    f.isDefined.value()(0) should be (false)
    f.isDefined.value()(1) should be (true)
  }

  it should "provide a term representing its domain" in {
    val f = for (i <- 0 ~~ 2) yield 2 / i
    f.funDom.value() should be (Set(1))
  }

  "A state" should "support boolean queries" in {
    val p = 'p of 0 ~~ 4 |-> Bools
    val query = for (i <- 0 ~~ 4) yield p(i)
    val data = state(p.atom(0) -> true, p.atom(1) -> false, p.atom(2) -> true, p.atom(3) -> false)
    val result = data.query(query)
    result should be (Good(Set(0,2)))
  }

  def maxProduct = Max.ByMessagePassing(_:Term[Double],MaxProduct.run(_,1))
  def bruteForce = Max.ByBruteForce(_:Term[Double])

  def maximizer(newMaximizer: => (Term[Double] => Max)) {
    it should "find argmax, gradient, and max value of a linear term" in {
      val w = 'w of Vectors
      val i = 'i of 0 ~~ 3
      val term = (unit(i) dot w) + 4.0
      val max = newMaximizer(term)
      val arg = new DenseVector(Array(0.0, 0.0, 3.0))
      val at = max.at(arg)
      at.value should be(7.0)
      at.argmax should be(state(i -> 2))
      at.subGradient should equal (unit(2).value()) (decided by vectorEq)
    }
  }

  "Max Product" should behave like maximizer(maxProduct)
  "Brute Force" should behave like maximizer(bruteForce)

  "An exhaustive argmaxer" should "find the maximizing assignment of a term" ignore {
    val x = 'x of Bools
    val y = 'y of Bools
    val model = I(x && !y)
    val argmax = Inference.exhaustiveArgmax(model).state()
    argmax(x) should be(true)
    argmax(y) should be(false)
  }

  "A MaxProduct argmaxer" should "find the maximizing assignment of a chain" ignore {
    val x = 'x of Bools
    val y = 'y of Bools
    val z = 'z of Bools
    val model = I(x |=> y) + I(y |=> z) + I(x)
    val expected = Inference.exhaustiveArgmax(model).state()
    val actual = Inference.maxProductArgmax(1)(model).state()
    actual should be(expected)
  }

  "A MaxProduct argmaxer" should "provide argmax feature vectors for linear models" ignore {
    val x = 'x of 0 ~~ 2
    val y = 'y of 0 ~~ 2
    val w = 'w of Vectors
    val index = new Index
    val weights = index.createDenseVector(Seq(0) -> -1.0, Seq(1) -> 1.0)()
    val model = {(unit(x) + unit(y)) dot w} | w -> weights

    val expected = Inference.exhaustiveArgmax(model)
    val actual = Inference.maxProductArgmax(1)(model)

    actual.state() should be(expected.state())
    actual.feats()(0) should be(expected.feats()(0))
    actual.feats()(1) should be(expected.feats()(1))

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

  "Normalizing linear models" should "result in flat vector and double terms with merged lambda abstractions" in {
    val n = 'n of Ints
    val word = 'word of (0 ~~ n |-> Strings)
    val chunk = 'chunk of (0 ~~ n |-> Strings)
    val weights = 'weights of Vectors
    val key = new Index()
    val bias = vsum(for (i <- 0 ~~ n as "i") yield unit(key('bias, chunk(i))))
    val wordChunk = vsum(for (i <- 0 ~~ n as "i") yield unit(key('wordChunk, word(i), chunk(i))))
    val trans = vsum(for (i <- 0 ~~ (n - 1) as "i") yield unit(key('trans, chunk(i), chunk(i + 1))))
    val hard1 = dsum(for (i <- 0 ~~ n as "i") yield log(I(chunk(i) === "O")))
    val hard2 = dsum(for (i <- 0 ~~ n as "i") yield log(I(chunk(i) === "B-NP")))

    val feat = bias + wordChunk + trans
    val model = (feat dot weights) + hard1 + hard2

    val actual = TermConverter.normalizeLinearModel(model, chunk.allAtoms)
    val expected = dsum(
      vsum(
        vsum(for (i <- 0 ~~ n as "i") yield vsum(unit(key('bias, chunk(i))), unit(key('wordChunk, word(i), chunk(i))))),
        vsum(for (i <- 0 ~~ (n - 1) as "i") yield unit(key('trans, chunk(i), chunk(i + 1))))
      ) dot weights,
      dsum(for (i <- 0 ~~ n as "i") yield dsum(log(I(chunk(i) === "O")), log(I(chunk(i) === "B-NP"))))
    )

    actual should be(expected)

  }

}

object CustomEqualities {
  def eps = 0.0002

  import Tolerance._
  import TripleEquals._

  implicit val vectorEq = new Equality[Vector] {
    def areEqual(a: Vector, b: Any): Boolean = (a,b) match {
      case (v1:SparseVector, v2:SingletonVector) => v1.activeDomain.forall(i => v1(i) === v2(i) +- eps)
      case (v1: Vector, v2:Vector) => v1.length == v2.length && v1.activeDomain.forall(i => a(i) === v1(i) +- eps)
      case _ => false
    }
  }
}