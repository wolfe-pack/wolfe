package ml.wolfe.term

import ml.wolfe.WolfeSpec
import org.scalautils.Explicitly

/**
 * @author riedel
 */
class TermSpecs extends WolfeSpec with Explicitly {

  import ml.wolfe.term.TermImplicits._

  "An vector variable term" should {
    "evaluate to a vector" in {
      val x = Dom.vectors(2).variable("x")
      val result = x(vector(1.0, 2.0))
      result should equal(vector(1.0, 2.0))
    }

    "provide its constant gradient" in {
      val x = Dom.vectors(2).variable("x")
      val result = x.gradient(x, vector(2.0, 1.0))
      result should equal(vector(1.0, 1.0))
    }
  }

  "A Tuple2Var term" should {
    "evaluate to a tuple2" in {
      val dom = Dom.doubles x Dom.doubles
      val x = dom.variable("x")
      val result = x((1.0, 2.0))
      result should be(1.0, 2.0)
    }

    "provide its first argument" in {
      val dom = Dom.doubles x Dom.doubles
      val x = dom.variable("x")
      val arg1 = x._1
      arg1((2.0, 1.0)) should be(2.0)
    }
  }

  "A dot product term" should {
    "evaluate to the value of a dot product" in {
      val x = Dom.vectors(2).variable("x")
      val dot = x dot x
      val result = dot(vector(2.0, 3.0))
      result should be(13.0)
    }

    "provide its gradient for identical variables" in {
      val x = Dom.vectors(2).variable("x")
      val dot = x dot x
      val result = dot.gradient(x, vector(2.0, 3.0))
      result should equal(vector(4.0, 6.0))
    }

    "provide its gradient for different variables " in {
      val x = Dom.vectors(2).variable("x")
      val y = Dom.vectors(2).variable("y")
      val dot = x dot y
      dot.gradient(x, vector(2.0, 3.0), vector(1.0, 2.0)) should equal (vector(1.0,2.0))
      dot.gradient(y, vector(2.0, 3.0), vector(1.0, 2.0)) should equal (vector(2.0,3.0))
    }
  }

  "A sigmoid function" should {
    "provide a logistic loss matrix factorization objective" in {
      val x = Dom.vectors(2).variable("x")
      val y = Dom.vectors(2).variable("y")
      val term = sigm(x dot y)
      term(vector(1.0,2.0),vector(2.0,3.0)) should equal (ml.wolfe.util.Math.sigmoid(8.0))
    }
  }

}
