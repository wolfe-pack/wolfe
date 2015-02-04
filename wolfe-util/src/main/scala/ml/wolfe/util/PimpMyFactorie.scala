package ml.wolfe.util

import cc.factorie.la._

import scala.language.implicitConversions

/**
 * @author rockt
 */
object PimpMyFactorie {
  //FIXME: for some reason this methods is not applied implicitly
  implicit def applyElementwise(fun: Double => Double): (Tensor => Tensor) = {
    (tensor: Tensor) =>
      for ((ix, value) <- tensor.activeElements) tensor.update(ix, fun(value))
      tensor
  }

  implicit class PimpedTensor(self: Tensor) {
    def toPrettyString: String = self match {
      //case sparse: SparseDoubleSeq => sparse.activeElements.map(t => t._1 + "\t" + t._2).mkString("\n")
      case tensor1: Tensor1 => tensor1.asArray.mkString("\n")
      case tensor2: Tensor2 => (0 until tensor2.dim1).map(row => (0 until tensor2.dim2).map(col => tensor2(row, col)).mkString(" ")).mkString("\n")
      case tensor3: Tensor3 =>
        (0 until tensor3.dim2).map(row => (0 until tensor3.dim1).map(layer =>
          (0 until tensor3.dim3).map(col => tensor3(layer, row, col)).mkString(" ")
        ).mkString(" | ")).mkString("\n")
    }
    def toDimensionsString: String = self match {
      case tensor1: Tensor1 => tensor1.dim1.toString
      case tensor2: Tensor2 => s"${tensor2.dim1}×${tensor2.dim2}"
      case tensor3: Tensor3 => s"${tensor3.dim1}×${tensor3.dim2}×${tensor3.dim3}"
    }
    def vectorization: Tensor1 = new DenseTensor1(self.asArray)

    /**
     * Two tensors are equal if the have the same dimensions and values
     */
    def ===(obj: scala.Any): Boolean = (obj, self) match {
      case (other: Tensor1, self: Tensor1) =>
        if (other.dim1 != self.dim1) false
        else {
          for (i <- 0 until self.dim1)
            if (self(i) != other(i)) return false
          true
        }
      case (other: Tensor2, self: Tensor2) =>
        if (other.dim1 != self.dim1) false
        if (other.dim2 != self.dim2) false
        else {
          for {
            i <- 0 until self.dim1
            j <- 0 until self.dim2
          }
            if (self(i,j) != other(i,j)) return false
          true
        }
      case (other: Tensor3, self: Tensor3) =>
        if (other.dim1 != self.dim1) false
        if (other.dim2 != self.dim2) false
        if (other.dim3 != self.dim3) false
        else {
          for {
            i <- 0 until self.dim1
            j <- 0 until self.dim2
            k <- 0 until self.dim3
          }
            if (self(i,j,k) != other(i,j,k)) return false
          true
        }
      case _ => self.equals(obj)
    }
  }

  implicit class PimpedTensor1(self: Tensor1) {
    def t: Tensor2 = new DenseTensor2(Array(self.asArray))
    def slice(from: Int, to: Int): Tensor1 = new DenseTensor1(self.asArray.slice(from, to))
    def *(tensor2: Tensor2): Tensor1 = tensor2.leftMultiply(self)

    def <>(tensor1: Tensor1): Tensor2 = self.outer(tensor1).asInstanceOf[Tensor2]
  }

  /**
   * Pimped tensor2 with dim1 = rows, dim2 = columns
   */
  implicit class PimpedTensor2(self: Tensor2) {
    /**
     * Returns the transpose of the matrix
     */
    def t: Tensor2 = {
      new DenseTensor2(self) {
        override protected def _initialArray: Array[Double] = self.asArray
        override val dim1 = self.dim2
        override val dim2 = self.dim1
        override def apply(i: Int, j: Int): Double = self.apply(j, i)
        override def *(t: Tensor1): Tensor1 = {
          assert(dim2 == t.dim1, "Dimensions don't match: " + dim2 + " " + t.dim1)
          val newT = new DenseTensor1(dim1)
          val newArray = newT.asArray
          t match {
            case t: DenseTensor =>
              val tArr = t.asArray
              var col = 0
              while (col < tArr.length) {
                val v = tArr(col)
                var row = 0
                while (row < dim1) {
                  newArray(row) += (apply(row, col) * v)
                  row += 1
                }
                col += 1
              }
            case t: SparseTensor =>
              val tActiveDomainSize = t.activeDomainSize
              val tIndices = t._indices
              val tValues = t._valuesSeq
              var ti = 0
              while (ti < tActiveDomainSize) {
                val col = tIndices(ti)
                val v = tValues(ti)
                var row = 0
                while (row < dim1) {
                  newArray(row) += (apply(row, col) * v)
                  row += 1
                }
                ti += 1
              }
            case _ =>
              throw new Error("tensor type neither dense nor sparse: " + t.getClass.getName)
          }
          newT
        }
      }
    }



    //TODO: make this more efficient
    def multiply(other: Tensor2): Tensor2 = {
      require(self.dim2 == other.dim1, s"${self.dim1}x${self.dim2} * ${other.dim1}x${other.dim2}")
      val tmp = new DenseTensor2(self.dim1, other.dim2)
      for {
        i <- 0 until self.dim1
        j <- 0 until other.dim2
      } tmp.update(i, j, (for (k <- 0 until self.dim2) yield self(i, k) * other(k, j)).sum)
      tmp
    }

    //rockt: inefficient?
    def reshape(rows: Int, columns: Int): Tensor2 = {
      require(rows * columns == self.dim1 * self.dim2)
      new DenseTensor2(self.asSeq.grouped(columns).toArray)
    }

    /**
     * Updates the ith column with tensor1
     */
    def update(i: Int, tensor1: Tensor1) = {
      require(self.dim1 == tensor1.dim1)
      for (j <- 0 until self.dim1) self.update(j, i, tensor1(j))
    }

    def mul(value: Double): Tensor2 = (self * value).asInstanceOf[Tensor2]

    def getRow(ix: Int): Tensor1 =
      new DenseTensor1((for (i <- 0 until self.dim2) yield self(ix, i)).toArray)

    //only works if tensor2 is a SparseBinaryTensor2
    def getSparseRow(ix: Int): SparseTensor1 = {
      val matrix = self.asInstanceOf[SparseBinaryTensor2]
      val v = new SparseTensor1(self.dim2)

      val minIx = ix * self.dim2
      val maxIx = (ix + 1) * self.dim2

      val elems = matrix.activeElements.filter(p => minIx <= p._1 && p._1 < maxIx)

      elems.foreach(p => {
        val (ix, value) = p
        v.update(ix % self.dim2, value)
      })

      v
    }
  }

  /**
   * Pimped tensor3 with dim1 = layers, dim2 = rows, dim3 = columns
   */
  implicit class PimpedTensor3(self: Tensor3) {
    /**
     * Multitplies the tensor with a vector in mode 1, i.e., inner product with every mode 1 (tube) fiber.
     * TODO: generalize this to mode 2 and mode 3
     * TODO: this method is the performance bottleneck: use DenseLayeredTensor3 and pick the vectors you need!
     * TODO: is there a parallel implementation for this?
     */
    def firstModeVectorProduct(tensor1: Tensor1): Tensor2 = tensor1 match {
      case t: SparseTensor =>
        //FIXME: this is currently not general, since it only works for calculating the 2*1 score
        require(self.dim2 == 2 && self.dim3 == 1)
        val result = new DenseTensor2(self.dim2, self.dim3)
        var sum0 = 0.0
        var sum1 = 0.0

        t.activeElements.foreach(elem => {
          val (ix, value) = elem
          sum0 += self(ix, 0, 0) * value
          sum1 += self(ix, 1, 0) * value
        })

        result.update(0, 0, sum0)
        result.update(1, 0, sum1)
        result
      case _ =>
        //println(tensor1.getClass)
        require(self.dim1 == tensor1.dim1, s"${self.toDimensionsString} * ${tensor1.toDimensionsString}")
        val tensor2 = new DenseTensor2(self.dim2, self.dim3)
        var i = 0
        var j = 0
        var k = 0
        while(j<self.dim2) {
          k = 0
          while(k<self.dim3) {
            i = 0
            val inner = new DenseTensor1(self.dim1)
            while (i < self.dim1) {
              inner.update(i,self(i,j,k))
              i+=1
            }
            tensor2.update(j, k, tensor1 dot inner)
            k+=1
          }
          j+=1
        }

        //      for {
        //        j <- 0 until self.dim2
        //        k <- 0 until self.dim3
        //      } {
        //        val inner = new DenseTensor1((for (i <- 0 until self.dim1) yield self(i, j, k)).toArray)
        //        tensor2.update(j, k, tensor1 dot inner)
        //      }
        tensor2
    }

    def secondModeVectorProduct(tensor1: Tensor1): Tensor2 = {
      require(self.dim2 == tensor1.dim1)
      val tensor2 = new DenseTensor2(self.dim1, self.dim3)
      var i = 0
      var j = 0
      var k = 0
      while(i<self.dim1) {
        k = 0
        while(k<self.dim3) {
          j = 0
          val inner = new DenseTensor1(self.dim2)
          while (j < self.dim2) {
            inner.update(j, self(i,j,k))
            j+=1
          }
          tensor2.update(i, k, tensor1 dot inner)
          k+=1
        }
        i+=1
      }
      tensor2
    }

    def thirdModeVectorProduct(tensor1: Tensor1): Tensor2 = {
      require(self.dim3 == tensor1.dim1)
      val tensor2 = new DenseTensor2(self.dim1, self.dim2)
      var i = 0
      var j = 0
      var k = 0
      while(i<self.dim1) {
        j = 0
        while(j<self.dim2) {
          k = 0
          val inner = new DenseTensor1(self.dim3)
          while (k < self.dim3) {
            inner.update(k, self(i,j,k))
            k+=1
          }
          tensor2.update(i, j, tensor1 dot inner)
          j+=1
        }
        i+=1
      }
      tensor2
    }

    def *(tensor1: Tensor1): Tensor2 = firstModeVectorProduct(tensor1)

    def mul(value: Double): Tensor3 = (self * value).asInstanceOf[Tensor3]

    /**
     * Multiplies a tensor by a matrix in mode n
     * rockt: needed for tucker decomposition
     */
    def nModeMatrixProduct(mode: Int, tensor2 : Tensor2): Tensor3 = {
      require(tensor2.dim2 == self.dimensions(mode))
      val tensor3 = new DenseTensor3(???, ???, ???)
      //TODO
      tensor3
    }


    def update(i: Int, tensor2: Tensor2): Unit = update1(i, tensor2)

    //update in mode 1
    def update1(i: Int, tensor2: Tensor2): Unit = {
      require(self.dim2 == tensor2.dim1)
      require(self.dim3 == tensor2.dim2)
      for {
        j <- 0 until self.dim2
        k <- 0 until self.dim3
      } self.update(i, j, k, tensor2(j, k))
    }

    //updates in mode 2
    def update2(j: Int, tensor2: Tensor2): Unit = {
      require(self.dim1 == tensor2.dim1)
      require(self.dim3 == tensor2.dim2)
      for {
        i <- 0 until self.dim1
        k <- 0 until self.dim3
      } self.update(i, j, k, tensor2(i, k))
    }

    //updates in mode 3
    def update3(k: Int, tensor2: Tensor2): Unit = {
      require(self.dim1 == tensor2.dim1 && self.dim2 == tensor2.dim2,
        s"can't update a ${self.dim1}×${self.dim2}×${self.dim3} tensor in mode 3 using a ${tensor2.dim1}×${tensor2.dim2} matrix")

      for {
        i <- 0 until self.dim1
        j <- 0 until self.dim2
      } self.update(i, j, k, tensor2(i, j))
    }


    def matricization(mode: Int): Tensor2 = ??? //TODO

    //rockt: this is slightly different compared to Kolda et al. (2009)
    def getFrontalSlice(i: Int): Tensor2 = {
      val matrix = new DenseTensor2(self.dim2, self.dim3)
      for {
        j <- 0 until self.dim2
        k <- 0 until self.dim3
      } matrix update(j, k, self(i, j, k))
      matrix
    }

    def getHorizontalSlice(j: Int): Tensor2 = {
      val matrix = new DenseTensor2(self.dim1, self.dim3)
      for {
        i <- 0 until self.dim1
        k <- 0 until self.dim3
      } matrix.update(i, k, self(i, j, k))
      matrix
    }

    def getLateralSlice(k: Int): Tensor2 = {
      val matrix = new DenseTensor2(self.dim1, self.dim2)
      for {
        i <- 0 until self.dim1
        j <- 0 until self.dim2
      } matrix.update(i, j, self(i, j, k))
      matrix
    }
  }

  implicit def tensorToTensor1(tensor: Tensor): Tensor1 = tensor match {
    case tensor1: Tensor1 => tensor1
    case tensor2: Tensor2 if tensor2.dim1 == 1 || tensor2.dim2 == 1 => new DenseTensor1(tensor2.asArray)
    case _ => throw new scala.MatchError("I don't know how to transform this into a Tensor1: " + tensor)
  }

  implicit def tensor1ToTensor2(tensor1: Tensor1): Tensor2 = {
    val tensor2 = new DenseTensor2(tensor1.dim1, 1) {
      _setArray(tensor1.asArray)
    }
    tensor2
  }

  //TODO: speed this up!
  def tensor3ToTensor2(tensor3: Tensor3): Tensor2 = {
    require(tensor3.dim3 == 1)
    val matrix = new DenseTensor2(tensor3.dim2, tensor3.dim1)
    for {
      i <- 0 until tensor3.dim1
      j <- 0 until tensor3.dim2
    } matrix update(j, i, tensor3(i, j, 0))

    matrix
  }

  //TODO: speed this up!
  def tensorToTensor3(tensor: Tensor): Tensor3 = tensor match {
    case tensor2: Tensor2 =>
      val tensor3 = new DenseTensor3(tensor2.dim2, tensor2.dim1, 1)
      for {
        i <- 0 until tensor2.dim1
        j <- 0 until tensor2.dim2
      } tensor3.update(j, i, 0, tensor2(i, j))
      tensor3
    case _ => throw new scala.MatchError("I don't know how to transform this into a Tensor3: " + tensor)
  }

  def featureMatrixToTensor3(tensor: Tensor, featureDim: Int): Tensor3 = tensor match {
    case tensor2: Tensor2 =>
      //val numActive = tensor2.activeElements.size
      val tensor3 = new SparseIndexedTensor3(featureDim + 1, tensor2.dim1, 1)
      /*{
        super.ensureCapacity(numActive)
        override def ensureCapacity(cap: Int): Unit = true
      }*/
      //println("outer tensor: " + tensor2.toDimensionsString)
      //println("new tensor: " + tensor3.toDimensionsString)
      //println("outer: " + tensor2.toPrettyString)
      for {
        i <- tensor2.activeDomain1
        j <- tensor2.activeDomain2
      } {
        //println(i,j)
        tensor3.update(j, i, 0, tensor2(i, j))
      }
      tensor3
  }

  val TENSOR3_ONE = {
    val one = new DenseTensor3(1,1,1)
    one update (0, 1.0)
    one
  }
}