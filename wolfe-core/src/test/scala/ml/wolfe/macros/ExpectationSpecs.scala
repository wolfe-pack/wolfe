package ml.wolfe.macros

import ml.wolfe.macros.OptimizedOperators._
import ml.wolfe.{BeliefPropagation, Wolfe, BruteForceOperators, WolfeSpec}

/**
 * @author Sebastian Riedel
 */
class ExpectationSpecs extends WolfeSpec {

  import ml.wolfe.Wolfe._


  "return constant expectations for constant stats " in {
    val actual = OptimizedOperators.expect(bools) { bernoulli(0.7) } { _ => oneHot(1) }
    actual should equal(Vector(1 -> 1.0))
  }

  "return the expectations of a bernoulli variable" in {
    val actual = OptimizedOperators.expect(bools) { bernoulli(0.7) } { oneHot(_) }
    val expected = Vector(true -> 0.7, false -> 0.3)
    actual.keys.foreach { k => actual(k) should be (expected(k) +- 0.0001)}
    // actual should equal(Vector(true -> 0.7, false -> 0.3))
  }

  "return the expectations of a one-node sample space and atomic objective" in {
    val actual = OptimizedOperators.expect(0 until 5) { _.toDouble } { oneHot(_) }
    val expected = BruteForceOperators.expect(0 until 5) { _.toDouble } { oneHot(_) }
    actual should equal(expected)
  }

  "return the single variable expectations of a two node case class sample space, one observation and atomic objective" in {
    case class Data(x: Boolean, y: Boolean)
    def model(d: Data) = I(!d.x || d.y)
    def stats(d: Data) = oneHot('x -> d.x) + oneHot('y -> d.y)
    val actual = OptimizedOperators.expect(Wolfe.all(Data) filter (_.x)) { model } { stats }
    val expected = BruteForceOperators.expect(Wolfe.all(Data) filter (_.x)) { model } { stats }
    actual should equal(expected)
  }

  "return the exact expectations of single node variables in a linear chain" in {
    def space = seqsOfLength(5, Range(0, 3))
    def stats(seq:Seq[Int]) = sum(0 until seq.size) { i => oneHot(i -> seq(i))}
    @LogZByInference(BeliefPropagation.sumProduct(1))
    def potential(seq: Seq[Int]) = {
      val local = sum (0 until seq.size) (i => i * I(seq(i) == i))
      val pairs = sum (0 until seq.size - 1)(i => I(seq(i) == seq(i + 1)))
      local + pairs
    }
    val actual = OptimizedOperators.expect(space) { potential } { stats }
    val expected = BruteForceOperators.expect(space) { potential } { stats }
    actual.keys.foreach { k => actual(k) should be (expected(k) +- 0.0001)}
 //   actual should equal (expected)
  }



}
