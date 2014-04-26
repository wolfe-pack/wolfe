package ml.wolfe

import Wolfe._
/**
 * @author Sebastian Riedel
 */
class VectorSpecs extends WolfeSpec {

  "A vector" should {
    "be creatable using one-hot vector expressions" in {
      val v = oneHot(1,3.0)
      v(1) should be (3.0)
      v(2) should be (0.0)
    }
    "be creable using a direct constructor" in {
      val v = Vector('A -> 2.0, 'B -> 1.0)
      v('A) should be (2.0)
      v('B) should be (1.0)
      v('C) should be (0.0)
    }


  }

}
