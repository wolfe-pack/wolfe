package ml.wolfe

import cc.factorie.la._
import cc.factorie.util.{DoubleSeq, RangeIntSeq, SparseDoubleSeq}
import breeze.linalg.{CSCMatrix, Counter, DenseMatrix, SparseVector}
import breeze.collection.mutable.SparseArray

/**
 * User: rockt
 * Date: 11/20/13
 * Time: 10:55 AM
 */
package object neural {
  type BreezeVector = breeze.linalg.SparseVector[Double]
  type BreezeMatrix = CSCMatrix[Double]
  type BreezeTensor = Counter[(Int, Int, Int), Double]

  trait Vector {
    def +(that: Vector): Vector
    def dot(that: Vector): Double
    def toArray: Array[Double]
    def *(that: Vector) = this dot that
  }

  implicit class FactorieVectorWrapper(val factorieVector: FactorieVector) extends Vector {
    implicit def cast(vector: Vector): FactorieVector = vector match { case v: FactorieVectorWrapper => v.factorieVector }
    implicit def tensor1ToVector(tensor1: Tensor1): Vector = new FactorieVector(tensor1._values)
    def +(that: Vector): Vector = factorieVector + that
    def dot(that: Vector): Double = factorieVector dot that
    def toArray: Array[Double] = factorieVector.toArray
  }

  implicit class BreezeVectorWrapper(val breezeVector: BreezeVector) extends Vector {
    implicit def cast(vector: Vector): BreezeVector = vector match { case v: BreezeVectorWrapper => v.breezeVector }
    def +(that: Vector): Vector = ??? // breezeVector + that
    def dot(that: Vector): Double = ??? // breezeVector dot that
    def toArray: Array[Double] = breezeVector.toArray
  }

  trait Matrix {
    def +(that: Matrix): Matrix
    def *(that: Vector): Vector
  }

  implicit class FactorieMatrixWrapper(val factorieMatrix: FactorieMatrix) extends Matrix {
    implicit def cast(matrix: Matrix): FactorieMatrix = matrix match { case m: FactorieMatrix => m.factorieMatrix }
    implicit def cast(vector: Vector): FactorieVector = vector match { case v: FactorieVectorWrapper => v.factorieVector }
    implicit def tensorToMatrix(tensor: Tensor): Matrix = tensor match { case m: FactorieMatrix => m }
    implicit def tensor1ToVector(tensor1: Tensor1): Vector = new FactorieVector(tensor1._values)
    def +(that: Matrix): Matrix = factorieMatrix + that
    def *(that: Vector): Vector = factorieMatrix * that
  }

  implicit class BreezeMatrixWrapper(val breezeMatrix: BreezeMatrix) extends Matrix {
    implicit def cast(matrix: Matrix): BreezeMatrix = matrix match { case m: BreezeMatrixWrapper => m.breezeMatrix }
    def +(that: Matrix): Matrix = ???
    def *(that: Vector): Vector = ???
  }

  implicit def tensor1ToVector(tensor1: Tensor1): FactorieVector = new FactorieVector(tensor1.asArray)
  implicit def doubleSeqToVector(doubleSeq: DoubleSeq): FactorieVector = new FactorieVector(doubleSeq)
  implicit def arrayToVector(array: Array[Double]): FactorieVector = new FactorieVector(array)
  //FIXME: Dangerous, since it might make code unreadable!
  implicit def tensor2ToArray(tensor2: Tensor2): Array[Double] = tensor2._values
  implicit def seqToVector(as: Seq[Double]): FactorieVector = new FactorieVector(as.toArray)

  def ew(a: FactorieVector)(function: Double => Double): FactorieVector = a.map(function)
  //def ew(a: BreezeVector)(function: Double => Double): BreezeVector = breeze.generic.UFunc(function)(a)

  def vectorToBreezeVector(vector: FactorieVector): BreezeVector = {
    val (sparseIndices, sparseValues) = vector.activeElements.toList.unzip
    new BreezeVector(sparseIndices.toArray, sparseValues.toArray, vector.dim1)
  }
  def matrixToBreezeMatrix(matrix: FactorieMatrix): BreezeMatrix =
    CSCMatrix.create(matrix.dim2, matrix.dim1, matrix.asArray).t
  def tensorToBreezeTensor(tensor: FactorieTensor): BreezeTensor = {
    val breezeTensor = Counter[(Int,Int,Int), Double]()
    for {
      i <- 0 until tensor.dim1
      matrix = tensor.inner(i)
      j <- 0 until tensor.dim2
      vector = matrix.inner(j)
      (index, value) <- vector.activeElements
    } breezeTensor.update((i,j,index), value)
    breezeTensor
  }

  def breezeVectorToVector(breezeVector: BreezeVector): FactorieVector = {
    val vector = new FactorieVector(breezeVector.length)
    for ((i, value) <- breezeVector.activeIterator)
      vector.update(i, value)
    vector
  }
  def breezeMatrixToMatrix(breezeMatrix: BreezeMatrix): FactorieMatrix = {
    val matrix = new FactorieMatrix(breezeMatrix.rows, breezeMatrix.cols)
    for {
      ((i,j),value) <- breezeMatrix.activeIterator
    } matrix update (i,j,value)
    matrix
  }
  def breezeTensorToTensor(breezeTensor: BreezeTensor): FactorieTensor = {
    val keys = breezeTensor.activeKeysIterator.toList
    val dim3 = keys.sortBy(-_._1).head._1 + 1 //FIXME
    val dim2 = keys.sortBy(-_._2).head._2 + 1
    val dim1 = keys.sortBy(-_._3).head._3 + 1 //FIXME
    val tensor = new FactorieTensor(dim1, dim2, dim3)
    for {
      ((i,j,k),value) <- breezeTensor.activeIterator
    } tensor update (i,j,k,value)
    tensor
  }

  class FactorieVector(dim: Int) extends SparseIndexedTensor1(dim) {
    //FIXME: why doesn't get this method inherited from DenseTensor1?
    def this(a:Array[Double]) = { this(a.length); this := a }
    def this(a:DoubleSeq) = { this(a.length); this := a }
    def toPrettyString: String = _values.toList.mkString(" ")
    //TODO: we need a dot product between sparse vectors
    def dot(that: FactorieVector): Double = {
      val indices = this._indices.toSet intersect that._indices.toSet
      var sum = 0.0
      for (ix <- indices) sum += this(ix) * that(ix)
      sum
    }
  }

  class FactorieMatrix(dim1: Int, dim2: Int) extends DenseLayeredTensor2(dim1, dim2, new FactorieVector(_)) {
    def toPrettyString: String =
      (0 until dim2).map(row => (0 until dim1).map(col => this(col,row)).mkString(" ")).mkString("\n")
  }

  class FactorieTensor(dim1: Int, dim2: Int, dim3: Int)
    extends Dense1LayeredTensor3(dim1, dim2, dim3, (innerdim1,innerdim2) => new FactorieMatrix(innerdim1,innerdim2)) {
    def toPrettyString: String = (0 until dim3).map(row => (0 until dim1).map(layer =>
      (0 until dim2).map(col => this(layer,col,row)).mkString(" ")
    ).mkString(" | ")).mkString("\n")
  }

  type OneHotVector = SingletonBinaryTensor1

  trait Dense1LayeredTensorLike3 extends Tensor3 with SparseDoubleSeq {
    def activeDomainSize = activeDomain.size
    def newTensor2: (Int,Int) => FactorieMatrix
    def activeDomain1 = new RangeIntSeq(0, dim1)
    def activeDomain2 = new RangeIntSeq(0, dim2)
    def activeDomain3 = new RangeIntSeq(0, dim3)
    def activeDomain = new RangeIntSeq(0, length) // Actually more sparse than this
    def ensureDimensionsMatch(t:FactorieTensor): Unit = t match {
      case t:Tensor3 => ensureDimensions(t.dim1, t.dim2, t.dim3)
      case _ => throw new Error("Tensor ranks do not match.")
    }
    def ensureDimensions(d1:Int, d2:Int, d3:Int): Unit = ???
    protected val _inners = new Array[FactorieMatrix](dim1) // Array.fill(dim1*dim2)(newTensor1(dim3)) // TODO We shouldn't pre-fill this array; leave it sparse
    override def apply(i:Int, j:Int, k:Int): Double = {
      assert(i*dim2+j < dim1*dim2, "len="+length+" dim1="+dim1+" dim2="+dim2+" dim3="+dim3+" i="+i+" j="+j+" k="+k)
      val t1 = _inners(i) //rockt: this guy is a matrix
      if (t1 ne null) t1.apply(j,k) else 0.0
    }
    def isDense = false
    def apply(i:Int): Double = apply(i/dim2/dim3, (i/dim3)%dim2, i%dim3)
    override def update(i:Int, v: Double): Unit = update(i/dim2/dim3, (i/dim3)%dim2, i%dim3, v)
    override def update(i:Int, j:Int, k:Int, v:Double): Unit = {
      var in = _inners(i)
      if (in eq null) { in = newTensor2(dim2,dim3); _inners(i) = in }
      in.update(k, v)
    }
    def update(i:Int, matrix: FactorieMatrix): Unit = _inners(i) = matrix //rockt
    override def zero(): Unit = { val len = _inners.length; var i = 0; while (i < len) { val in = _inners(i); if (in ne null) inner(i).zero(); i += 1 }}
    /** Get the inner Tensor1 at first two dimensions index i,j.  Create it if necessary. */
    def inner(i:Int): FactorieMatrix = { var in = _inners(i); if (in eq null) { in = newTensor2(dim2,dim3); _inners(i) = in }; in }
    def inners = _inners //rockt
    override def +=(i:Int, incr:Double): Unit = inner(index1(i)).+=(index2(i),index3(i), incr)
    override def +=(i1:Int, i2:Int, i3:Int, incr:Double): Unit = inner(i1).+=(i3, incr)
    override def =+(a:Array[Double], offset:Int, f:Double): Unit = {
      val len = _inners.length
      var i = 0
      while (i < len) {
        val in = _inners(i)
        if (in ne null) {
          inner(i).=+(a, offset+i*dim3, f)
        }
        i += 1
      }
    }
    def foreachActiveElement(f: (Int, Double) => Unit) {
      val len = _inners.length
      var i = 0
      while (i < len) {
        val in = _inners(i)
        if (in ne null) {
          in.foreachActiveElement((ii, vv) => f(i*dim3+ii, vv))
        }
        i += 1
      }
    }

    //FIXME: implementations missing and some are probably buggy
    override def dot(ds:DoubleSeq): Double = ds match {
      case t:SingletonBinaryTensor3 => apply(t.singleIndex1, t.singleIndex2, t.singleIndex3)
      case t:SingletonTensor3 => apply(t.singleIndex1, t.singleIndex2, t.singleIndex3) * t.singleValue
      case t:Singleton2BinaryLayeredTensorLike3 => { val i = _inners(t.singleIndex1*dim2+t.singleIndex2); if (i ne null) i.dot(t.inner) else 0.0 }
      case t:Singleton2LayeredTensorLike3 => { val i = _inners(t.singleIndex1*dim2+t.singleIndex2); if (i ne null) i.dot(t.inner) * t.singleValue1 * t.singleValue2 else 0.0 }
      case t:Dense2LayeredTensor3 => ???
      case t:SparseBinaryTensor3 => {
        /*println("Dense2LayeredTensorLike3 this.length="+length+" t.length="+t.length+" dims="+t.dimensions.toSeq);*/
        val tArr = t.activeDomain.array; val tLen = tArr.length; var s = 0.0; var i = 0
        while (i < tLen) { s += apply(tArr(i)); i += 1 }
        s
      }
      case t:DenseTensor => {
        var res = 0.0
        var i = 0
        while (i < _inners.length) {
          _inners(i) match {
            case in:DenseTensor =>
              var j = 0
              while (j < in.length) {
                res += t(singleIndex(i / dim2, i % dim2, j))*in(j)
                j += 1
              }
            case in:SparseIndexedTensor => in.foreachActiveElement((x,v) => res += t(singleIndex(i / dim2, i % dim2, x))*v)
          }
          i += 1
        }
        res
      }
      case t:SparseIndexedTensor => {var res = 0.0; t.foreachActiveElement((i, x) => res += this(i)*x); res}
      case t:DoubleSeq => assert(false, t.getClass.getName + " doesn't match") ; 0.0
    }
    override def +=(t:DoubleSeq, f:Double): Unit = t match {
      case t:SingletonBinaryTensor3 => +=(t.singleIndex1, t.singleIndex2, t.singleIndex3, f)
      case t:SingletonTensor3 => +=(t.singleIndex1, t.singleIndex2, t.singleIndex3, f * t.singleValue)
      case t:Singleton2LayeredTensorLike3 => ???
      case t:Dense2LayeredTensorLike3 => ???
      case t:SparseBinaryTensor3 => { var s = 0.0; t.foreachActiveElement((i,v) => +=(i, f)) }
      case t:Singleton2BinaryLayeredTensorLike3 => ???
      case t:DenseTensor => {
        var i = 0
        while (i < _inners.length) {
          inner(i) match {
            case in:DenseTensor =>
              var j = 0
              while (j < in.length) {
                in(j) += t(singleIndex(i / dim2, i % dim2, j))*f
                j += 1
              }
            case in:SparseIndexedTensor => in.foreachActiveElement((x,v) => in(x) += t(singleIndex(i / dim2, i % dim2, x))*f)
          }
          i += 1
        }
      }
      case t:SparseIndexedTensor => t.foreachActiveElement((i, x) => this(i) += f*x)
      case t:DoubleSeq => assert(false, t.getClass.getName + " doesn't match")
    }
  }

  class Dense1LayeredTensor3(val dim1: Int, val dim2: Int, val dim3: Int, val newTensor2: (Int,Int) => FactorieMatrix) extends Dense1LayeredTensorLike3 {
    def this(dim1: Int, dim2: Int, dim3: Int) = this(dim1, dim2, dim3, (innerdim1, innerdim2) => new FactorieMatrix(innerdim1, innerdim2))
    override def blankCopy = new Dense1LayeredTensor3(dim1, dim2, dim3, newTensor2)
    override def copy = {
      val c = new Dense1LayeredTensor3(dim1, dim2, dim3, newTensor2)
      val innerCopy = _inners.map(t => if (t == null) null else t.copy)
      System.arraycopy(innerCopy, 0, c._inners, 0, innerCopy.length)
      c
    }
  }
}
