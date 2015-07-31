package ml.wolfe

import cc.factorie.la.{SparseTensor1, DenseTensor1}

import scala.pickling.Pickler

/**
 * Default Specs for Wolfe.
 *
 * @author Sebastian Riedel
 */
class PicklerSpecs extends WolfeSpec {

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

    "store and load sparse tensor 1 objects in json" in {
      import scala.pickling.json._
      val vector = new SparseVector(10)
      vector(2) = 2.0
      vector(1) = 1.0
      val pkl = vector.pickle
      val result = pkl.unpickle[SparseTensor1]
      factorieVectorEq.areEqual(result,vector) should be (true) //todo: why does implicit eq not work here?
    }

    "store vectors within case classes" in {
      import scala.pickling.json._

      case class Params(weigths:DenseTensor1,dim:Int)
      val params = Params(new DenseTensor1(Array(1.0,2.0)),3)
      val pkl = params.pickle
      val result = pkl.unpickle[Params]
      result.dim should be (3)
      result.weigths(0) should be (1.0)
      result.weigths(1) should be (2.0)
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

  "A simple index serializer" should {
    "store and load a simple index to json" in {
      import scala.pickling.json._

      val a:Symbol = 'A
      a.pickle.unpickle[Symbol].asInstanceOf[Any] should be ('A:Any)

      val index = new SimpleIndex
      index(1)
      index("A")
      index("B")
      index("B" -> "C")

      val result = index.pickle.unpickle[SimpleIndex]
      result.toMap should be (index.toMap)

    }
  }

}
