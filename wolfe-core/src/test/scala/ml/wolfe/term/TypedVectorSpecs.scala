package ml.wolfe.term

import ml.wolfe.WolfeSpec

import scala.util.Random

/**
 * @author riedel
 */
class TypedVectorSpecs extends WolfeSpec {

  import ml.wolfe.term.TermImplicits._

  implicit val random = new Random(0)

  "A typed vector" should {
    "support access of elements by keys" in {
      val x = TypedVectors(Bools).Var
      val b = Bools.Var
      x(b).eval(x := vector(1,2), b := false) should be (1.0)
      x(b).eval(x := vector(1,2), b := true) should be (2.0)



    }

  }

}

