package ml.wolfe.macros

import ml.wolfe.{MaxProduct, BruteForceOperators, Wolfe, WolfeSpec}

/**
 * @author Sebastian Riedel
 */
class OptimizeByInferenceSpecs extends WolfeSpec {

  import OptimizedOperators._
  import Wolfe._


  "An argmax operator" should {

    "return the argmax of a one-node sample space and atomic objective" in {
      val actual = argmax(0 until 5) { _.toDouble }
      val expected = BruteForceOperators.argmax(0 until 5) { _.toDouble }
      actual should be(expected)
    }


    "return the argmax of a one-node sample space and atomic objective with implicit domain" in {
      val actual = argmax { over[Boolean] of (I(_)) }
      val expected = BruteForceOperators.argmax { over[Boolean] of (I(_)) }
      actual should be(expected)
    }

    "return the argmax of a two node case class sample space, one observation and atomic objective" in {
      case class Data(x: Boolean, y: Boolean)
      val actual = argmax { over(Wolfe.all(Data)) of (d => I(!d.x || d.y)) st (_.x) }
      val expected = BruteForceOperators.argmax { over(Wolfe.all(Data)) of (d => I(!d.x || d.y)) st (_.x) }
      actual should be(expected)
    }

    "return the argmax of a two node case class sample space, one observation and atomic objective (new style)" in {
      case class Data(x: Boolean, y: Boolean)
      val actual = argmax(Wolfe.all(Data) filter (_.x)) { d => I(!d.x || d.y) }
      val expected = BruteForceOperators.argmax(Wolfe.all(Data) filter (_.x)) { d => I(!d.x || d.y) }
      actual should be(expected)
    }


    "use the algorithm in the maxBy annotation" in {
      //todo: can we find a better way to check whether an annotation was used
      case class Data(x: Boolean, y: Boolean, z: Boolean)
      implicit def data = Wolfe.all(Data)
      @OptimizeByInference(MaxProduct(_, 10))
      def model(d: Data) = I(d.x && d.y) + I(d.y && !d.z) + I(d.z && !d.x)
      @OptimizeByInference(MaxProduct(_, 2))
      def twoIterations(d: Data) = model(d)
      @OptimizeByInference(MaxProduct(_, 1))
      def oneIteration(d: Data) = model(d)
      val afterOneIter = argmax(data) { oneIteration }
      val afterTwoIter = argmax(data) { twoIterations }
      val afterTenIter = argmax(data) { model }
      val expected = BruteForceOperators.argmax(data) { model }
      afterOneIter should not be expected
      afterTwoIter should be(expected)
      afterTenIter should be(expected)
    }

    "find the optimal solution of a linear chain " in {
      def space = seqs(5, Range(0, 3))
      @OptimizeByInference(MaxProduct(_, 1))
      def potential(seq: Seq[Int]) = {
        val local = sum { over(0 until seq.size) of (i => i * I(seq(i) == i)) }
        val pairs = sum { over(0 until seq.size - 1) of (i => I(seq(i) == seq(i + 1))) }
        local + pairs
      }
      val actual = argmax { over(space) of potential }
      val expected = BruteForceOperators.argmax { over(space) of potential }

      actual should be(expected)
    }


  }

}
