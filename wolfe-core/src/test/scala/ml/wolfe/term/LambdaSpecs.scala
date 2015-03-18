package ml.wolfe.term

import ml.wolfe.WolfeSpec

/**
 * @author riedel
 */
class LambdaSpecs extends WolfeSpec {

  import ml.wolfe.term.TermImplicits._

  "A lambda function" should {
    "create a typed scala function using a term" in {
      val f = lambda(Bools){x => I(x)}
      f(true) should be (1.0)
    }

  }


}
