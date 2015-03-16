package ml.wolfe.term

import ml.wolfe.{FactorieVector, SimpleIndex, WolfeSpec}

/**
 * @author riedel
 */
class OneHotTermSpecs extends WolfeSpec {

  import ml.wolfe.term.TermImplicits._

  "A one-hot term" should {
    "create one hot vectors" in {
      implicit val V = Vectors(3)
      val i = Ints.Var
      val v = Doubles.Var
      val o = oneHot(i,v)

      o(i << 0, v << 2.0) should equal (vector(2.0,0.0,0.0))
      o(i << 2, v << -1.0) should equal (vector(0.0,0.0,-1.0))

    }

  }


}
