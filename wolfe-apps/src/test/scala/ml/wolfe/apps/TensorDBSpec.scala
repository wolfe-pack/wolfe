package ml.wolfe.apps

import org.scalatest.{FunSpec, Matchers, FreeSpec, WordSpec}

import scala.util.Random

/**
 * Created by rockt on 19/09/2014.
 */
class TensorDBSpec extends WordSpec with Matchers {
  "A tensor DB" should {
    "can build indices from structured objects" in {
      val db = new TensorDB()
      import db.EmptyIx

      db.getKey(1) shouldBe (1, EmptyIx, EmptyIx)
      db.getKey(List(1)) shouldBe (1, EmptyIx, EmptyIx)
      db.getKey(Seq(1,2,3)) shouldBe (1,2,3)
      db.getKey((1,2,3)) shouldBe (1,2,3)
      db.getKey(Seq(1,2,3,4,5)) shouldBe (1,2,Seq(3,4,5))
      db.getKey((1, (2, 3))) shouldBe (1,2,3)
      db.getKey((1, (2, (3,4)))) shouldBe (1,2, (3,4))
    }

    "keep multiple indices if the cell is indexed by a Seq or Tuple" in {
      val db = new TensorDB()
      db += Cell((1, 2))

      db.get((1,2)) shouldBe Some(Cell((1, 2)))
      db.get(1, 2) shouldBe Some(Cell((1, 2)))

      db += Cell((1, 2, 3))
      db.get(1,2,3) shouldBe Some(Cell((1,2,3)))

      db += Cell(Seq("a"))
      db.get("a") shouldBe Some(Cell(Seq("a")))

      db += Cell(Seq("a", "b"))
      db.get("a", "b") shouldBe Some(Cell(Seq("a", "b")))
    }

    "be a matrix if cells are indexed by exactly two indices" in {
      val db = new TensorDB()
      db.isMatrix shouldBe false

      db += Cell("r")
      db.isMatrix shouldBe false

      db += Cell(Seq("r1", "e1"))
      db.isMatrix shouldBe true

      db += Cell(Seq("r2", "e1", "e2"))
      db.isMatrix shouldBe false
    }

    "be usable in a natural way for a knowledge base with binary relations" in {
      val matrix = new TensorDB()
      matrix.sampleTensor(10,5)
      println(matrix.toVerboseString)

      val tensor = new TensorDB()
      tensor.sampleTensor(10,5,5)
      println(tensor.toVerboseString)
    }
  }
}
