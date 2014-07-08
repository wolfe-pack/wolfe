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
      val dims = Seq('a -> 3, 'b -> 2).toMap

      val xs: Array[Int] = new Array[Int](6)
      for (i <- 0 until 6) xs(i) = i
      val Xs = LabelledTensor.onExistingArray(Array('a, 'b), dims, xs)

      val ys: Array[Int] = new Array[Int](6)
      for (i <- 0 until 6) ys(i) = i
      val Ys = LabelledTensor.onExistingArray(Array('b, 'a), dims, ys)

      Xs += Ys
      xs.toSeq shouldEqual Seq(0, 4, 3, 7, 6, 10)
    }

    "be correct when the second operand is in a subspace" in {
      def dims(x:Int) = 2
      val xs: Array[Int] = new Array[Int](16)
      for (i <- 0 until 16) xs(i) = i
      val Xs = LabelledTensor.onExistingArray(Array(0, 1, 2, 3), dims, xs)

      val ys: Array[Int] = new Array[Int](4)
      for (i <- 0 until 4) ys(i) = i
      val Ys = LabelledTensor.onExistingArray(Array(0, 2), dims, ys)

      Xs += Ys
      xs.toSeq shouldEqual Seq(0, 1, 3, 4, 4, 5, 7, 8, 10, 11, 13, 14, 14, 15, 17, 18)
    }

    "be correct when the labels of destination are permuted" in {
      def dims(l:String) = 2
      val Xs = LabelledTensor.onExistingArray(Array("d1", "d2", "d3"), dims, (0 until 8).toArray)
      val Ys = LabelledTensor.onExistingArray(Array("d2", "d1", "d3"), dims, (0 until 8).toArray)
      val Zs = LabelledTensor.onExistingArray(Array("d2", "d3", "d1"), dims, (0 until 8).toArray)

      Xs.elementWiseToDestination[Int, Int](Ys, _ + _ , Zs)
      Zs.array.toSeq shouldEqual Seq(0, 6, 2, 8, 6, 12, 8, 14)
    }

    "throw an exception if the dimensions are incorrect" in {
      def dims(l:Symbol) = 2
      val Xs = LabelledTensor.onExistingArray(Array('a, 'b), dims, (0 until 4).toArray)
      val Ys = LabelledTensor.onExistingArray(Array('a, 'b, 'c), dims, (0 until 8).toArray)
      val Zs = LabelledTensor.onExistingArray(Array('a, 'b, 'c, 'd), dims, (0 until 16).toArray)

      try {Xs += Ys; fail() }
      catch { case e:LabelledTensorDimensionError => }

      try {Ys.elementWiseToDestination[Int, Int](Xs, _+_, Zs); fail()}
      catch { case e:LabelledTensorDimensionError => }
    }
  }

  "A fold operation" should {
    "correctly fold across multiple dimensions" in {
      def dims(l:Int) = 2
      val Xs = LabelledTensor.onExistingArray(Array(0, 1, 2, 3), dims, (0 until 16).toArray)
      val folded = Xs.fold[Int](Array(1, 3), 0, _+_)
      folded.array.toSeq shouldEqual Seq(20, 24, 36, 40)
    }

    "throw an exception if the dimensions are incorrect" in {
      def dims(l:Symbol) = 3
      val Xs = LabelledTensor.onExistingArray(Array('a, 'b), dims, (0 until 9).toArray)

      try {Xs.fold[Int](Array('a, 'b, 'c), 0, _+_); fail() }
      catch { case e:LabelledTensorDimensionError => }

      try {Xs.fold[Int](Array('c), 0, _+_); fail() }
      catch { case e:LabelledTensorDimensionError => }
    }

  }
}
