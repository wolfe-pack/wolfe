package ml.wolfe

import cc.factorie.la._

/**
 * @author riedelcastro
 */
trait Mat2 {

  def dim1: Int

  def dim2: Int

  def apply(i: Int, j: Int): Double

  def +=(that: Mat2): Unit

  def :=(that: Mat2): Unit

  def :=(scale: Double): Unit

  def *=(scale: Double): Unit

  def update(i: Int, j: Int, value: Double): Unit

  def *(that: Vect2): Vect2

  def t: Mat2

  def copy:Mat2

  def randomize(eps: => Double): Unit


}

class FactorieMat(val self: Tensor2) extends Mat2 {

  import FactorieVect._

  def dim1 = self.dim1

  def dim2 = self.dim2

  private def matchFMat[T](that: Mat2)(proc: Tensor2 => T): T = that match {
    case f: FactorieMat => proc(f.self)
    case _ => throw new UnsupportedOperationException
  }

  def *=(scale: Double) = self *= scale


  def randomize(eps: => Double) = {
    for (i1 <- 0 until self.dim1; i2 <- 0 until self.dim2)
      this(i1,i2) += eps
  }

  def *(that: Vect2) = matchFVect(that)(v => new FactorieVect(self * v))

  def update(i: Int, j: Int, value: Double) = self(i, j) = value

  def :=(that: Mat2) = ???

  def :=(scale: Double) = self := scale

  def +=(that: Mat2) = ???

  def apply(i: Int, j: Int) = self.apply(i, j)

  def copy = new FactorieMat(self.copy)

  def t = {
    new FactorieMat(new DenseTensor2(self) {
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
    })

  }
}
