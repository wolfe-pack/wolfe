package ml.wolfe.macros

import ml.wolfe.{BruteForceOperators, Wolfe, WolfeSpec}

/**
 * @author Sebastian Riedel
 */
class ArgmaxSpecs extends WolfeSpec {

  import OptimizedOperators._

  "An argmax operator" should {
    "return the argmax of a one-node sample space and atomic objective " in {
      import Wolfe._
      val actual = argmax { over(Range(0, 5)) of (_.toDouble) }
      val expected = BruteForceOperators.argmax { over(Range(0, 5)) of (_.toDouble) }
      actual should be(expected)
    }

    "return the argmax of a one-node sample space and atomic objective with implicit domain" in {
      import Wolfe._
      val actual = argmax { over[Boolean] of (I(_)) }
      val expected = BruteForceOperators.argmax { over[Boolean] of (I(_)) }
      actual should be(expected)
    }

    "return the argmax of a two node case class sample space, one observation and atomic objective" in {
      import Wolfe._
      case class Data(x: Boolean, y: Boolean)
      val actual = argmax { over(Wolfe.all(Data)) of (d => I(!d.x || d.y)) st (_.x) }
      val expected = BruteForceOperators.argmax { over(Wolfe.all(Data)) of (d => I(!d.x || d.y)) st (_.x) }
      actual should be(expected)
    }
  }

}
