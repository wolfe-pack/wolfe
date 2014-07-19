package ml.wolfe.macros

import ml.wolfe.{Wolfe, BruteForceOperators, WolfeSpec}

/**
 * @author Sebastian Riedel
 */
class ExpectationSpecs extends WolfeSpec {

  import ml.wolfe.Wolfe._

  "return the expectations of a one-node sample space and atomic objective" in {
    val actual = OptimizedOperators.expect(0 until 5) { _.toDouble } { oneHot(_) }
    val expected = BruteForceOperators.expect(0 until 5) { _.toDouble } { oneHot(_) }
    actual should equal(expected)
  }

  "return the argmax of a two node case class sample space, one observation and atomic objective" in {
    case class Data(x: Boolean, y: Boolean)
    def model(d: Data) = I(!d.x || d.y)
    def stats(d: Data) = oneHot('x -> d.x) + oneHot('y -> d.y)
    val actual = OptimizedOperators.expect(Wolfe.all(Data) filter (_.x)) { model } { stats }
    val expected = BruteForceOperators.expect(Wolfe.all(Data) filter (_.x)) { model } { stats }
    actual should equal(expected)
  }


}
