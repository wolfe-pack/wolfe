package ml.wolfe.compiler.nd4s

import org.nd4j.linalg.api.ndarray.INDArray
import org.nd4s.NDArrayEvidence
import org.nd4s.Implicits._
import org.nd4j.linalg.ops.transforms.Transforms._


/**
 * @author rockt
 */
object PimpMyND4S {
  implicit class PimpedNDArray[A <: INDArray](self: A) {
    def dimsString = dims.mkString("x")

    def dims = self.shape()

    def cols = self.columns()

    def t = self.T

    def isTensor = !(self.isVector || self.isMatrix)

    def :*[B](that: INDArray)(implicit ev: NDArrayEvidence[A, B]): A = self * that

    def outer[B](that: INDArray)(implicit ev: NDArrayEvidence[A, B]): A = {
      require(self.isVector)
      require(that.isVector)
      (self.isColumnVector, that.isRowVector) match {
        case (true, true) => self ** that
        case (false, true) => self.T.asInstanceOf[A] ** that
        case (true, false) => self ** that.T
        case (false, false) => self.asInstanceOf[A] ** that.T
      }
    }

    def toArray: Array[Double] = ???

    def mul[B](that: INDArray)(implicit ev: NDArrayEvidence[A, B]): B = {
      require(self.isMatrix)
      require(that.isVector)
      (that ** self).asInstanceOf[B]
    }
  }
}
