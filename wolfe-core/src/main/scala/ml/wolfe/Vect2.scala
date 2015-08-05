package ml.wolfe

import cc.factorie.la._
import ml.wolfe.term.MutableSingletonTensor1
import scalaxy.loops._
import scala.language.postfixOps


/**
 * @author riedelcastro
 */
trait Vect2 {
  def dim: Int

  def apply(i: Int): Double

  def +=(that: Vect2): Unit

  def :=(that: Vect2): Unit

  def :=(scale: Double): Unit

  def *=(scale: Double): Unit

  def dot(that: Vect2): Double

  def :*(that: Vect2, scale: Double)

  def update(i: Int, value: Double)

  def randomize(eps: => Double): Unit

  def zero(): Unit

  def foreachActiveElement(f: (Int, Double) => Unit): Unit

}

object FactorieVect {
  def sparse(dim: Int) = new FactorieVect(new SparseTensor1(dim))

  def dense(dim: Int) = new FactorieVect(new DenseTensor1(dim))

  def dense(elements: Array[Double]) = new FactorieVect(new DenseTensor1(elements))
}

class FactorieVect(var self: Tensor1) extends Vect2 {
  def dim = self.dim1

  def apply(i: Int) = self(i)

  def randomize(eps: => Double) = {
    for (j <- 0 until self.length)
      self(j) += eps
  }

  private def matchFVect[T](that: Vect2)(proc: Tensor1 => T): T = that match {
    case f: FactorieVect => proc(f.self)
    case _ => throw new UnsupportedOperationException
  }

  def :=(scale: Double) = self := scale

  def *=(scale: Double) = self *= scale

  def zero() = self.zero()

  def dot(that: Vect2) = matchFVect(that)(ownDot(self, _))

  def update(i: Int, value: Double) = self(i) = value

  def :*(other: Vect2, scale: Double) = {
    self match {
      case s: SparseIndexedTensor1 =>
        s._makeReadable()
        for (i <- 0 until s._unsafeActiveDomainSize) {
          val index = s._indices(i)
          s._values(i) *= other(index) * scale
        }
      case _ =>
        self.foreachActiveElement((ix, v) => self.update(ix, v * other(ix) * scale))
    }

  }

  def foreachActiveElement(f: (Int, Double) => Unit) = self.foreachActiveElement(f)

  private def copyVector(v: Vect) = v match {
    case s: SingletonTensor1 => new SingletonTensor1(s.dim1, s.singleIndex, s.singleValue)
    case s: MutableSingletonTensor1 => new MutableSingletonTensor1(s.dim1, s.singleIndex, s.singleValue)
    case s: GrowableSparseHashTensor1 =>
      val result = new GrowableSparseHashTensor1(s.sizeProxy)
      result += s
      result
    case null => null
    case _ => v.copy
  }

  def +=(that: Vect2) = matchFVect(that) {
    fvect => (self, fvect) match {
      case (_, null) =>
      case (current: SparseIndexedTensor, arg: DenseTensor1) =>
        self = arg.copy
        self += current
      case (current: DenseTensor1, arg: SparseIndexedTensor) =>
        self += arg
      case (_, arg) =>
        self += arg
    }
  }

  def :=(that: Vect2) = matchFVect(that) {
    fvect => (self, fvect) match {
      case (_: DenseTensor1, target: SparseIndexedTensor) =>
        self = copyVector(target)
      case (_: SparseIndexedTensor, target: DenseTensor1) =>
        self = copyVector(target)
      case (_, _) =>
        self := fvect
    }
  }

  def sparseDot(s1: SparseIndexedTensor1, arg2: Vect) = {
    var result = 0.0
    for (i <- 0 until s1._unsafeActiveDomainSize optimized) {
      val index = s1._indices(i)
      val value = s1._values(i)
      result += value * arg2(index)
    }
    result
  }

  def ownDot(arg1: Vect, arg2: Vect) = {
    arg1 match {
      case s1: SparseIndexedTensor1 => sparseDot(s1, arg2)
      case _ =>
        arg2 match {
          case s2: SparseIndexedTensor1 => sparseDot(s2, arg1)
          case _ => arg1 dot arg2
        }
    }
  }


}