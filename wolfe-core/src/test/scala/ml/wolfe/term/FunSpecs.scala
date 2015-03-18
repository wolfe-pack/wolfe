package ml.wolfe.term

import ml.wolfe.WolfeSpec

/**
 * @author riedel
 */
class FunSpecs extends WolfeSpec {

  import ml.wolfe.term.TermImplicits._

  "A function creator" should {
    "create a typed scala function using a term" in {
      val f = fun(Bools){x => I(x)}
      f(true) should be (1.0)
    }
    "create a typed scala function with two arguments" in {
      val f = fun(Bools,Bools){(x,y) => I(x && y)}
      f(true,true) should be (1.0)
    }


  }


}
