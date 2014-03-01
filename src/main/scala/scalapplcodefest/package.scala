import cc.factorie.la.{SingletonTensor1, DenseTensor1, SparseTensor1, Tensor1}
import scalapplcodefest.legacy.value.Fun

/**
 * @author Sebastian Riedel
 */
package object scalapplcodefest {
  type Vector = Tensor1
  type SparseVector = SparseTensor1
  type DenseVector = DenseTensor1
  type SingletonVector = SingletonTensor1
  type AnyFunction = PartialFunction[Nothing,Any]
  type AnyFun = Fun[Nothing,Any]

}
