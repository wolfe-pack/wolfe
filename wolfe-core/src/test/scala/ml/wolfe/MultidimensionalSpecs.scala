package ml.wolfe

import ml.wolfe.util.Multidimensional._

/**
 * @author Luke
 */
class MultidimensionalSpecs extends WolfeSpec {

  "A cartestian product" should {
    "be in lexicographical order" in {
      val prod = cartesianProduct(Seq(Seq(1,2,3), Seq(4, 5)))
      prod shouldEqual Seq(Seq(1,4), Seq(1,5), Seq(2,4), Seq(2,5), Seq(3,4), Seq(3,5))
    }
  }

  "An elementwise operation" should {

    "be correct when the second operand is in the same space" in {
      val xs: Array[Int] = new Array[Int](6)
      for (i <- 0 until 6) xs(i) = i
      val dims = Seq('a -> 3, 'b -> 2).toMap
      val Xs = LabelledTensor.onExistingArray[Symbol, Int](Array('a, 'b), dims, xs)

      val ys: Array[Int] = new Array[Int](6)
      for (i <- 0 until 6) ys(i) = i
      val Ys = LabelledTensor.onExistingArray[Symbol, Int](Array('b, 'a), dims, ys)

      Xs += Ys
      xs.toSeq shouldEqual Seq(2, 6, 5, 9, 8, 12)
    }

    "be correct when the second operand is in a subspace" in {
      val xs: Array[Int] = new Array[Int](16)
      for (i <- 0 until 16) xs(i) = i
      val Xs = LabelledTensor.onExistingArray[Int, Int](Array(0, 1, 2, 3), x => 2, xs)

      val ys: Array[Int] = new Array[Int](4)
      for (i <- 0 until 4) ys(i) = i
      val Ys = LabelledTensor.onExistingArray[Int, Int](Array(0, 2), x => 2, ys)

      Xs += Ys
      xs.toSeq shouldEqual Seq(0, 1, 3, 4, 4, 5, 7, 8, 10, 11, 13, 14, 14, 15, 17, 18)
    }
  }

  "A fold operation" should {
    "correctly fold from 4 dimensions to 2 dimensions" in {
      val xs: Array[Int] = new Array[Int](16)
      for (i <- 0 until 16) xs(i) = i
      val Xs = LabelledTensor.onExistingArray[Int, Int](Array(0, 1, 2, 3), x => 2, xs)
      val folded = Xs.fold(Array(1, 3), 0, (acc:Int, x:Int) => acc + x)
      folded.array.toSeq shouldEqual Seq(20, 24, 36, 40)
    }
  }
}
