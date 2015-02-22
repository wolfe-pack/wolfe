package ml.wolfe

/**
 * @author riedel
 */
package object term {
  type DoubleVar = DoubleDom#DomVar
  type VectorVar = VectorDom#DomVar
  type MatrixVar = MatrixDom#DomVar
  type DiscVar[T] = DiscreteDom[T]#DomVar
  type Constant[D <: Dom] = D#Constant

  type TypedDom[T] = Dom { type Value = T}
  type DomWithTerm[T] = Dom { type Term = T}
  type DoubleTerm = Term[DoubleDom]
  type VectorTerm = Term[GenericVectorDom]
  type MatrixTerm = Term[MatrixDom]
  type BoolDom = DiscreteDom[Boolean]
  type BoolTerm = Term[BoolDom]
  type DiscreteTerm[T] = Term[DiscreteDom[T]]
//  type IntTerm = DiscreteTerm[Int]
  type DoubleSeqDom = SeqDom[DoubleDom]

}
