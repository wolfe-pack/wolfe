package scalapplcodefest

import org.scalatest.{WordSpec, Matchers}
import org.scalautils._
import Tolerance._
import org.scalautils._
import cc.factorie.optimize.{Perceptron, OnlineTrainer}
import scalapplcodefest.value.{AllOfType, Doubles, Vectors, Gen}
import scalapplcodefest.term._
import scala.Some
import scalapplcodefest.value.Gen
import scalapplcodefest.term.FunctionNotDefinedAt
import scalapplcodefest.term.Constant
import org.scalautils.Bad
import scalapplcodefest.term.VariableUndefined

/**
 * Set of specs.
 * @author Sebastian Riedel
 */
class Specs extends WordSpec with Matchers {

  def eps = 0.0002

  import TermDSL._
  import CustomEqualities._

  "A state" should {
    "provide values of variables or return None" in {
      val i = 'i of ints
      val j = 'j of ints
      val s = state(i -> 1)
      s.get(i) should be(Some(1))
      s.get(j) should be(None)
    }
    "turn values of variables to target values" in {
      val i = 'i of ints
      val s = state(i -> 1)
      val t = s.asTargets(Gen(Set(i)))
      t.get(Target(i)) should be(Some(1))
      t.get(i) should be(None)
    }

    "turn target values of variables to actual values" in {
      val i = 'i of ints
      val s = state(Target(i) -> 1)
      val t = s.target
      t.get(Target(i)) should be(None)
      t.get(i) should be(Some(1))
    }

    "provide a closed world view" in {
      val i = 'i of ints
      val j = 'j of ints
      val k = 'k of ints
      val s = state(i -> 1).closed(Set(j))
      s.get(i) should be (Some(1))
      s.get(j) should be (Some(j.default))
      s.get(k) should be (None)
      s.domain should be (Set(i,j))
    }

    "support boolean queries" in {
      val p = 'p of 0 ~~ 4 |-> bools
      val query = for (i <- 0 ~~ 4) yield p(i)
      val data = state(p.atom(0) -> true, p.atom(1) -> false, p.atom(2) -> true, p.atom(3) -> false)
      val result = data.query(query)
      result should be(Good(Set(0, 2)))
    }
  }


  "A constant" should {
    "evaluate to its value" in {
      val c = Constant(10)
      c.eval(state()) should be(Good(10))
    }
  }

  "A variable" should {
    "evaluate to the value it is assigned to in the state" in {
      val x = 'x of ints
      x.eval(state(x -> 0)) should be(Good(0))
    }
    "evaluate to an Undefined object if no value is assigned to it" in {
      val x = 'x of ints
      x.eval(State.empty) should be(Bad(VariableUndefined(x, State.empty)))
    }
    "evaluate to an Undefined object if the assigned value is outside the domain" in {
      val x = 'x of 0 ~~ 2
      x.eval(state(x -> 3)) should be(Bad(ValueOutsideOfDomain(x, state(x -> 3))))
    }
  }

  "A predicate" should {
    "evaluate to a function controlled by the assignments to the predicate's atoms" in {
      val p = 'p of 0 ~~ 2 |-> bools
      val f = p.value(state(p.atom(0) -> false, p.atom(1) -> true))
      f(0) should be(false)
      f(1) should be(true)
    }
    "return as variables all ground atoms of the predicate" in {
      val p = 'p of 0 ~~ 2 |-> bools
      p.variables should be(Set(p.atom(0), p.atom(1)))
    }
  }

  "A function application" should {
    "evaluate to the value of the function term applied to the value of the argument term" in {
      val f = bools.neg
      val app = f(false)
      app.eval(state()) should be(Good(true))
    }
    "evaluate to a Undefined object outside of the functions domain" in {
      val f = for (i <- 0 ~~ 2) yield i
      val app = f(3)
      app.eval(State.empty) should be(Bad(FunctionNotDefinedAt(app, State.empty)))
    }
    "return ground atoms as variables if the function is a predicate" in {
      val p = 'p of bools |-> bools
      val term = p(true)
      term.variables should be(Set(p.atom(true)))
    }
    "return no free variables if the function is a predicate and the application is conditioned to have all variables set" in {
      val p = 'p of bools |-> bools
      val term = p(true) | p.atom(true) -> true
      term.variables should be(Set.empty)
    }
  }

  "A lambda abstraction" should {
    "evaluate to a function that returns the value of the body for a state where the lambda variable is assigned to the function argument" in {
      val l = for (x <- bools) yield !x
      val f = l.value()
      f(true) should be(false)
      f(false) should be(true)
    }
    "evaluate to functions with different domains if the variable domain is state-dependent" in {
      val n = 'n of ints
      val f = for (i <- 0 ~~ n) yield i
      f.value(state(n -> 1)).funDom should be(Set(0))
      f.value(state(n -> 3)).funDom should be(Set(0, 1, 2))
    }
    "evaluate to a function with tuple arguments if the variable is a tuple" in {
      val f = for ((x, y) <- c(bools, bools)) yield x || y
      f.value()(false, false) should be(false)
      f.value()(false, true) should be(true)
    }
    "evaluate to a curried function if nested with another lambda abstraction" in {
      val f = for (x <- bools; y <- bools) yield x || y
      f.value()(false)(false) should be(false)
      f.value()(false)(true) should be(true)
    }
  }

  "A conditioned term" should {
    "evaluate to the value its inner term would evaluate to if the condition was added to its state argument" in {
      val x = 'x of ints
      val y = 'y of ints
      val c = (x + y) | state(x -> 2)
      c.eval(state(y -> 1)) should be(Good(3))
    }
    "should not return free variables that are defined in its condition" in {
      val x = 'x of ints
      val y = 'y of ints
      val c = (x + y) | state(x -> 2)
      c.variables should be(Set(y))
    }
  }

  "A set term" should {
    "map each element in the set when mapped" in {
      val n = 'n of ints
      val s = 0 ~~ n
      val f = for (i <- ints) yield i + 1
      val m = s mappedBy f
      m.value(n -> 2) should be(Set(1, 2))
    }
    "filter out elements in the set if they match the predicate" in {
      val n = 'n of ints
      val s = 0 ~~ n
      val f = for (i <- ints) yield i === 1
      val m = s filteredBy f
      m.value(n -> 2) should be(Set(1))
    }
  }

  "A constant function term" should {
    "be acting like the function value when doing function application" in {
      val f = fun[String, Int]({case x => x.length})
      f.value()("123") should be(3)
    }
  }

  "A partial function term" should {
    "provide a definedAt function" in {
      val f = for (i <- 0 ~~ 2) yield 2 / i
      f.isDefined.value()(0) should be(false)
      f.isDefined.value()(1) should be(true)
    }
    "provide a term representing its domain" in {
      val f = for (i <- 0 ~~ 2) yield 2 / i
      f.funDom.value() should be(Set(1))
    }
  }

  "A sum" when {
    "given a sequence of argument terms" should {
      "evaluate to the sum of the denotations of its arguments" in {
        val d = 'd of doubles
        val s = doubles.sum(d, d, d + d)
        s.value(d -> 2.0) should be(8.0)
      }
    }

    "given a function argument" should {
      "evaluate to the sum of the values we get by applying the function to all values in its domain" in {
        val n = 'n of ints
        val s = ints.sum(for (i <- 0 ~~ n) yield i)
        s.value(n -> 4) should be(6)
      }
    }
    "given a curried function argument" should {
      "evaluate to the sum over all arguments to all functions the first abstraction yields" in {
        val n = 'n of ints
        val s = ints.sum(for (i <- 0 ~~ n; j <- 0 ~~ n) yield i + j)
        s.value(n -> 3) should be (18)
      }
    }
  }

  "An Iverson bracket" should {
    "denote 1.0 if the inner boolean term is true, and 0.0 for false" in {
      val i = 'i of ints
      val t = I(i === 2)
      t.value(i -> 2) should be (1.0)
      t.value(i -> 10) should be (0.0)
    }
  }


  "A unit vector" should {
    "evaluate to a vector that is active only at the component the index term evaluates to" in {
      val i = 'i of ints
      val d = 'd of doubles
      val v = unit(i, d).value(state(i -> 2, d -> 2.0))
      v(2) should be(2.0 +- eps)
      for (j <- v.activeDomain.asSeq; if j != 2) v(j) should be(0.0 +- eps)
    }
  }

  "A dot product" should {
    "evaluate to the dot product of the denotations of its arguments" in {
      val i = 'i of ints
      val v = 'v of doubles
      val d = unit(i,v) dot unit(i,v)
      d.value(i -> 1, v -> 2.0) should be (4.0)
    }
  }

  "An index" should {
    "evaluate to a unique and fixed integer index for each possible argument array" in {
      val index = new Index
      val i = 's of strings
      val b = 'b of bools
      val t1 = index(i, b)
      val t2 = index(i)
      t1.value(i -> "A", b -> true) should be(t1.value(i -> "A", b -> true))
      t1.value(i -> "A", b -> true) should not be t2.value(i -> "A", b -> true)
      t1.value(i -> "A", b -> true) should not be t1.value(i -> "B", b -> false)
    }
  }


  def maximizer(newMaximizer: => (Term[Double] => Max)) {
    "find argmax, gradient, and max value of a linear term" in {
      val w = 'w of vectors
      val i = 'i of 0 ~~ 3
      val term = (unit(i) dot w) + 4.0
      val max = newMaximizer(term)
      val arg = state(w -> new DenseVector(Array(0.0, 0.0, 3.0)))
      max.value(arg) should be(7.0)
      max.argmax.value(arg) should be(state(i -> 2))
      max.gradient.value(arg) should equal(unit(2).value())(decided by vectorEq)
    }

  }

  def maxProduct = Max.ByMessagePassing(_: Term[Double], MaxProduct.run(_, 1))
  def bruteForce = Max.ByBruteForce(_: Term[Double])


  "Max Product" should {
    behave like maximizer(maxProduct)
  }

  "Brute Force" should {
    behave like maximizer(bruteForce)
  }

  def gradientBasedMinizer(newMinimizer: => (Term[Double] => Vector)) {

    "find a minimum of a perceptron loss" in {
      val i = 'i of 0 ~~ 3
      val weights = 'w of vectors
      val model = unit(i) dot weights
      val gold = state(i -> 2)
      val loss = Max.ByBruteForce(model) - (model | gold)
      val result = newMinimizer(loss)
      loss.value(state(weights -> result)) should be(0.0 +- eps)
      val resultLoss = loss.value(state(weights -> result))
      println(result)
      println(resultLoss)

    }
  }

  def perceptronMinimizer = GradientBasedMinimizer.minimize(_: Term[Double], new OnlineTrainer(_, new Perceptron, 5))

  "Perceptron Minimizer" should {
    behave like gradientBasedMinizer(perceptronMinimizer)
  }

  "Pushing down conditions" should {
    "move condition terms downward the term tree" in {
      val n = 'n of ints
      val p = 'p of (0 ~~ n |-> bools)
      val state = State(Map(n -> 2))
      val term = doubles.sum {for (i <- (0 ~~ n) as "i") yield I(p(i)) + 1.0} | state
      val actual = TermConverter.pushDownConditions(term)
      val pCond = 'p of (0 ~~ (n | state) |-> bools)
      val expected = doubles.sum {for (i <- (0 ~~ (n | state)) as "i") yield I(pCond(i) | state) + 1.0}
      actual should be(expected)
    }
  }

  "A Domain Collector" should {
    "collect values for dynamic domains from states" in {
      val Dom = 'Dom of Constant(new AllOfType[Set[String]])
      val d = 'd of Dom
      val p = 'p of 0 ~~ 2 |-> Dom
      val r = 'r of c(Dom, Dom) |-> bools
      val states = Seq(state(p.atom(0) -> "A"), state(p.atom(1) -> "B", r.atom("B", "C") -> true), state(d -> "D"))
      val domains = DomainCollector.collect(states)
      domains(Dom) should be(Set("A", "B", "C", "D"))
    }
  }

  "Unrolling images of lambda abstractions" should {
    "create sequences of terms, one for each element in the domain" in {
      val p = 'p of (0 ~~ 3 |-> bools)
      val term = doubles.sum {for (i <- 0 ~~ 3) yield I(p(i))}
      val expected = doubles.sum(I(p(0)), I(p(1)), I(p(2)))
      val actual = TermConverter.unrollLambdaImages(term)
      actual should be(expected)
    }
  }

  "Flattening terms" should {
    "replace trees of binary function applications with reductions of the function" in {
      val x = 'x of doubles
      val term = ((x + x) + (x + x)) + doubles.sum(x, x) + x
      val expected = doubles.sum(x, x, x, x, x, x, x)
      val actual = TermConverter.flatten(term, doubles.add)
      actual should be(expected)
    }
  }

  "Pushing down dot products" should {
    "replace dot products of vector sums with double sums of dot products" in {
      val w = 'w of vectors
      val f1 = vectors.sum(for (i <- (0 ~~ 2) as "i") yield unit(i))
      val f2 = vectors.sum(unit(0), unit(1))
      val term = (f1 + f2) dot w
      val flat = TermConverter.flatten(term, vectors.add)
      val actual = TermConverter.pushDownDotProducts(flat)
      val expected = doubles.sum(doubles.sum(for (i <- (0 ~~ 2) as "i") yield unit(i) dot w), unit(0) dot w, unit(1) dot w)
      actual should be(expected)
    }
  }

  "Grouping lambda abstractions" should {
    "group lambda abstractions over the same domain if they have the same hidden variables" in {
      val p = 'p of ints |-> ints
      val f1 = vectors.sum(for (i <- (0 ~~ 2) as "i") yield unit(p(i)))
      val f2 = vectors.sum(for (i <- (0 ~~ 2) as "i") yield unit(p(i) + 1))
      val term = vectors.sum(f1, f2)
      val actual = TermConverter.groupLambdas(term)
      val expected = vectors.sum(for (i <- (0 ~~ 2) as "i") yield vectors.sum(unit(p(i)), unit(p(i) + 1)))
      actual should be(expected)
    }
  }

}

object CustomEqualities {
  def eps = 0.0002

  import TripleEquals._

  implicit val vectorEq = new Equality[Vector] {
    def areEqual(a: Vector, b: Any): Boolean = (a, b) match {
      case (v1: SparseVector, v2: SingletonVector) => v1.activeDomain.forall(i => v1(i) === v2(i) +- eps)
      case (v1: Vector, v2: Vector) => v1.length == v2.length && v1.activeDomain.forall(i => a(i) === v1(i) +- eps)
      case _ => false
    }
  }
}