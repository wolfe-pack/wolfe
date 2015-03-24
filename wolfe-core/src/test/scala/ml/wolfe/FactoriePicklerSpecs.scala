package ml.wolfe

import cc.factorie.la.DenseTensor1

/**
 * Default Specs for Wolfe.
 *
 * @author Sebastian Riedel
 */
class FactoriePicklerSpecs extends WolfeSpec {

  import WolfePicklers._
  import scala.pickling.Defaults._

  "A dense tensor pickler" should {
    "store and load dense tensor 1 objects in json" in {
      import scala.pickling.json._

      val vector = new DenseTensor1(Array(1.0,2.0))
      val pkl = vector.pickle
      val result = pkl.unpickle[DenseTensor1]
      factorieVectorEq.areEqual(result,vector) should be (true) //todo: why does implicit eq not work here?
      //result should equal (vector)

    }
    "store and load dense tensor 1 objects in binary format" in {
      import scala.pickling.binary._

      val vector = new DenseTensor1(Array(1.0,2.0))
      val pkl = vector.pickle
      val result = pkl.unpickle[DenseTensor1]
      factorieVectorEq.areEqual(result,vector) should be (true) //todo: why does implicit eq not work here?
      //result should equal (vector)

    }

  }

}
