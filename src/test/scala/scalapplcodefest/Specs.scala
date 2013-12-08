package scalapplcodefest

import org.scalatest.{WordSpec, Matchers}
import org.scalautils._
import Tolerance._
import org.scalautils._
import cc.factorie.optimize.{Perceptron, OnlineTrainer}

/**
 * Set of specs.
 * @author Sebastian Riedel
 */
class Specs extends WordSpec with Matchers {

  def eps = 0.0002

  import TermImplicits._
  import CustomEqualities._

  "A constant" should {
    "evaluate to its value" in {
      val c = Constant(10)
      c.eval(state()) should be(Good(10))
    }
  }

  "A variable" should {
    "evaluate to the value it is assigned to in the state" in {
      val x = 'x of Ints
      x.eval(state(x -> 0)) should be(Good(0))
    }
    "evaluate to an Undefined object if no value is assigned to it" in {
      val x = 'x of Ints
      x.eval(State.empty) should be(Bad(VariableUndefined(x, State.empty)))
    }
    "evaluate to an Undefined object if the assigned value is outside the domain" in {
      val x = 'x of 0 ~~ 2
      x.eval(state(x -> 3)) should be(Bad(VariableUndefined(x, state(x -> 3))))
    }
  }

  "A function application" should {
    "evaluate to the value of the function term applied to the value of the argument term" in {
      val f = Logic.Neg.Term
      val app = f(false)
      app.eval(state()) should be(Good(true))
    }
    "evaluate to a Undefined object outside of the functions domain" in {
      val f = for (i <- 0 ~~ 2) yield i
      val app = f(3)
      app.eval(State.empty) should be(Bad(FunctionNotDefinedAt(app, State.empty)))
    }
    "return ground atoms as variables if the function is a predicate" in {
      val p = 'p of Bools |-> Bools
      val term = p(true)
      term.variables should be(Set(p.atom(true)))
    }
    "return no free variables if the function is a predicate and the application is conditioned to have all variables set" in {
      val p = 'p of Bools |-> Bools
      val term = p(true) | p.atom(true) -> true
      term.variables should be(Set.empty)
    }
  }

  "A lambda abstraction" should {
    "evaluate to a function that returns the value of the body for a state where the lambda variable is assigned to the function argument" in {
      val l = for (x <- Constant(Bools)) yield x
      val f = l.value()
      f(true) should be(true)
      f(false) should be(false)
    }
    "evaluate to a function with tuple arguments if the variable is a tuple" in {
      val f = for ((x, y) <- C(Bools, Bools)) yield x || y
      f.value()(false, false) should be(false)
      f.value()(false, true) should be(true)
    }
  }

  "A conditioned term" should {
    "evaluate to the value its inner term would evaluate to if the condition was added to its state argument" in {
      val x = 'x of Ints
      val y = 'y of Ints
      val c = (x + y) | state(x -> 2)
      c.eval(state(y -> 1)) should be (Good(3))
    }
    "should not return free variables that are defined in its condition" in {
      val x = 'x of Ints
      val y = 'y of Ints
      val c = (x + y) | state(x -> 2)
      c.variables should be (Set(y))
    }
  }

  "A set term" should {
    "map each element in the set when mapped" in {
      val n = 'n of Ints
      val s = 0 ~~ n
      val f = for (i <- Constant(Ints)) yield i + 1
      val m = s mappedBy f
      m.value(n -> 2) should be(Set(1, 2))
    }
    "filter out elements in the set if they match the predicate" in {
      val n = 'n of Ints
      val s = 0 ~~ n
      val f = for (i <- Constant(Ints)) yield i === 1
      val m = s filteredBy f
      m.value(n -> 2) should be(Set(1))
    }
  }

  "A constant function term" should {
    "be acting like the function when doing function application" in {
      val f = funTerm[String, Int]({case x => x.length})
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

  "A state" should {
    "provide values of variables or return None" in {
      val i = 'i of Ints
      val j = 'j of Ints
      val s = state(i -> 1)
      s.get(i) should be(Some(1))
      s.get(j) should be(None)
    }
    "turn values of variables to target values" in {
      val i = 'i of Ints
      val s = state(i -> 1)
      val t = s.asTargets(Gen(Set(i)))
      t.get(Target(i)) should be(Some(1))
      t.get(i) should be(None)
    }

    "turn target values of variables to actual values" in {
      val i = 'i of Ints
      val s = state(Target(i) -> 1)
      val t = s.target
      t.get(Target(i)) should be(None)
      t.get(i) should be(Some(1))
    }

    "support boolean queries" in {
      val p = 'p of 0 ~~ 4 |-> Bools
      val query = for (i <- 0 ~~ 4) yield p(i)
      val data = state(p.atom(0) -> true, p.atom(1) -> false, p.atom(2) -> true, p.atom(3) -> false)
      val result = data.query(query)
      result should be(Good(Set(0, 2)))
    }
  }

  def maximizer(newMaximizer: => (Term[Double] => Max)) {
    "find argmax, gradient, and max value of a linear term" in {
      val w = 'w of Vectors
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
      val weights = 'w of Vectors
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
      val n = 'n of Ints
      val p = 'p of (0 ~~ n |-> Bools)
      val state = State(Map(n -> 2))
      val term = dsum {for (i <- (0 ~~ n) as "i") yield I(p(i)) + 1.0} | state
      val actual = TermConverter.pushDownConditions(term)
      val pCond = 'p of (0 ~~ (n | state) |-> Bools)
      val expected = dsum {for (i <- (0 ~~ (n | state)) as "i") yield I(pCond(i) | state) + 1.0}
      actual should be(expected)
    }
  }

  "A Domain Collector" should {
    "collect values for dynamic domains from states" in {
      val Dom = 'Dom of Constant(new AllOfType[Set[String]])
      val d = 'd of Dom
      val p = 'p of 0 ~~ 2 |-> Dom
      val r = 'r of (Dom x Dom) |-> Bools
      val states = Seq(state(p.atom(0) -> "A"), state(p.atom(1) -> "B", r.atom("B", "C") -> true), state(d -> "D"))
      val domains = DomainCollector.collect(states)
      domains(Dom) should be(Set("A", "B", "C", "D"))
    }
  }

  "Unrolling images of lambda abstractions" should {
    "create sequences of terms, one for each element in the domain" in {
      val p = 'p of (0 ~~ 3 |-> Bools)
      val term = dsum {for (i <- 0 ~~ 3) yield I(p(i))}
      val expected = dsum(I(p(0)), I(p(1)), I(p(2)))
      val actual = TermConverter.unrollLambdaImages(term)
      actual should be(expected)
    }
  }

  "Flattening terms" should {
    "replace trees of binary function applications with reductions of the function" in {
      val x = 'x of Doubles
      val term = ((x + x) + (x + x)) + dsum(x, x) + x
      val expected = dsum(x, x, x, x, x, x, x)
      val actual = TermConverter.flatten(term, Math.DoubleAdd)
      actual should be(expected)
    }
  }

  "Pushing down dot products" should {
    "replace dot products of vector sums with double sums of dot products" in {
      val w = 'w of Vectors
      val f1 = vsum(for (i <- (0 ~~ 2) as "i") yield unit(i))
      val f2 = vsum(unit(0), unit(1))
      val term = (f1 + f2) dot w
      val flat = TermConverter.flatten(term, Math.VecAdd)
      val actual = TermConverter.pushDownDotProducts(flat)
      val expected = dsum(dsum(for (i <- (0 ~~ 2) as "i") yield unit(i) dot w), unit(0) dot w, unit(1) dot w)
      actual should be(expected)
    }
  }

  "Grouping lambda abstractions" should {
    "group lambda abstractions over the same domain if they have the same hidden variables" in {
      val p = 'p of Ints |-> Ints
      val f1 = vsum(for (i <- (0 ~~ 2) as "i") yield unit(p(i)))
      val f2 = vsum(for (i <- (0 ~~ 2) as "i") yield unit(p(i) + 1))
      val term = vsum(f1, f2)
      val actual = TermConverter.groupLambdas(term)
      val expected = vsum(for (i <- (0 ~~ 2) as "i") yield vsum(unit(p(i)), unit(p(i) + 1)))
      actual should be(expected)
    }
  }

  "Normalizing linear models" should {
    "result in flat vector and double terms with merged lambda abstractions" in {
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