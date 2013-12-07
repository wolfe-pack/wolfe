import cc.factorie.la.{SingletonTensor1, DenseTensor1, SparseTensor1, Tensor1}

/**
 * @author Sebastian Riedel
 */
package object scalapplcodefest {
  type Vector = Tensor1
  type SparseVector = SparseTensor1
  type DenseVector = DenseTensor1
  type SingletonVector = SingletonTensor1
  type AnyFunction = PartialFunction[Nothing,Any]
  type ValueAndGradient = (Double,Vector)
  type Inferencer = LinearModel => DenseVector => State => MutableInference
}
