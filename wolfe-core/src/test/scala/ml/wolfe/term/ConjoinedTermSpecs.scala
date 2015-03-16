package ml.wolfe.term

import ml.wolfe.{SimpleIndex, WolfeSpec}

/**
 * @author riedel
 */
class ConjoinedTermSpecs extends WolfeSpec {

  import ml.wolfe.term.TermImplicits._

  "A conjoined term" should {
    "provide a feature conjunction" in {
      implicit val V = Vectors(4)
      implicit val index = new SimpleIndex

      val i = Ints.Var
      val j = Ints.Var
      val o = oneHot(i,2.0) conjoin oneHot(j, 2.0)

      o(i << 0,j << 0) should equal (vector(4,0,0,0))
      o(i << 0,j << 1) should equal (vector(0,4,0,0))
      o(i << 1,j << 0) should equal (vector(0,0,4,0))
      o(i << 1,j << 1) should equal (vector(0,0,0,4))

    }

  }


}
