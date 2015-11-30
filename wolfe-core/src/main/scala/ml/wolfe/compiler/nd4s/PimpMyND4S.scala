package ml.wolfe.compiler.nd4s

import org.nd4j.linalg.api.ndarray.INDArray
import org.nd4s.NDArrayEvidence
import org.nd4s.Implicits._
import org.nd4j.linalg.ops.transforms.{Transforms => num}
import ml.wolfe._

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

    def :*[B](that: INDArray)(implicit ev: NDArrayEvidence[A, B]): A = {
      //fixme: get rid of this special case
      if (self.isColumnVector && that.isRowVector) (self.T * that).asInstanceOf[A]
      else self * that
    }

    def outer[B](that: INDArray)(implicit ev: NDArrayEvidence[A, B]): A = {
      require(self.isVector)
      require(that.isVector)
      (self.isColumnVector, that.isRowVector) match {
        case (true, true) => self ** that
        case (false, true) => self.T.asInstanceOf[A] ** that
        case (true, false) => self ** that.T
        case (false, false) => self.T.asInstanceOf[A] ** that.T
      }
    }

    //fixme: this is a terrible hack!
    def toArray: Array[Double] = self.data().array().asInstanceOf[Array[_]].map(_.toString.toDouble)
  }
}
