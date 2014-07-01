package ml.wolfe

import ml.wolfe.util.LabelledTensor

/**
 * Created by luke on 30/06/14.
 */
class LabelledTensorSpecs extends WolfeSpec {
  "An elementwise operation" should {
    "be correct when the argument has fewer dimensions" in {
      val xs: Array[Int] = new Array[Int](16)
      for (i <- 0 until 16) xs(i) = i
      val Xs = LabelledTensor.onExistingArray[Int, Int](Array(0, 1, 2, 3), x => 2, xs)

      val ys: Array[Int] = new Array[Int](4)
      for (i <- 0 until 4) ys(i) = i
      val Ys = LabelledTensor.onExistingArray[Int, Int](Array(0, 2), x => 2, ys)

      Xs.elementWiseOp[Int](Ys, _ + _)
      xs.toSeq shouldEqual Seq(0, 1, 3, 4, 4, 5, 7, 8, 10, 11, 13, 14, 14, 15, 17, 18)
    }
  }

  "A fold operation" should {
    "correctly fold 2 of 4 dimensions" in {
      val xs: Array[Int] = new Array[Int](16)
      for (i <- 0 until 16) xs(i) = i
      val Xs = LabelledTensor.onExistingArray[Int, Int](Array(0, 1, 2, 3), x => 2, xs)
      val folded = Xs.fold(Array(1, 3), 0, (acc:Int, x:Int) => acc + x)
      folded.array.toSeq shouldEqual Seq(20, 24, 36, 40)
    }
  }
}
