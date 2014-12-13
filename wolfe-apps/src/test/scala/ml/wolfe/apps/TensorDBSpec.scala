package ml.wolfe.apps

import ml.wolfe.apps.factorization.{Impl, TensorKB, Cell, TensorDB}
import org.scalatest.{FunSpec, Matchers, FreeSpec, WordSpec}

import scala.util.Random

/**
 * Created by rockt on 19/09/2014.
 */
class TensorDBSpec extends WordSpec with Matchers {
  "A tensor DB" should {
    "add and retrieve cells" in {
      val db = new TensorDB()
      //vector
      db += Cell(Seq("a"))
      db.get(Seq("a")) shouldBe Some(Cell(Seq("a")))

      db += Cell(Seq("a", "b"))
      db.get(Seq("a", "b")) shouldBe Some(Cell(Seq("a", "b")))

      //matrix
      db += Cell(1, 2)
      db.get(1, 2) shouldBe Some(Cell(1, 2))

      //tensor
      db += Cell(1, 2, 3)
      db.get(1,2,3) shouldBe Some(Cell(1,2,3))

    }

    "be a matrix if cells are indexed by exactly two indices" in {
      val db = new TensorDB()
      db.isMatrix shouldBe false

      db += Cell("r")
      db.isMatrix shouldBe false

      db += Cell("r1", "e1")
      db.isMatrix shouldBe true

      db += Cell("r2", "e1", "e2")
      db.isMatrix shouldBe false
    }

    "be usable in a natural way for a knowledge base with binary relations" in {
      val matrix = new TensorDB()
      matrix.sampleTensor(10,5)
      println(matrix.toVerboseString(showTrain = true))

      val tensor = new TensorDB()
      tensor.sampleTensor(10,5,5)

      println(tensor.toVerboseString(showTrain = true))
    }

    "be serializable and deserializable" in {
      val db = new TensorDB(5)
      db.sampleTensor(10, 10, 0, 0.1) //samples a matrix
      db += Impl("r3", "r4")
      db += Impl("r4", "r6")
      db += Impl("r6", "r2")

      val fg = db.toFactorGraph
      fg.build()

      db.serialize("/tmp/serialized/")


      val db2 = new TensorDB(5)
      db2.deserialize("/tmp/serialized/")

      db.cells.size == db2.cells.size
      db.keys1.size == db2.keys1.size
      db.keys2.size == db2.keys2.size
      db.keys3.size == db2.keys3.size
    }
  }
}

