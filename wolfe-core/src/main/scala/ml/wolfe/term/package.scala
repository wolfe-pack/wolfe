package ml.wolfe

/**
 * @author riedel
 */
package object term {
  type DoubleVar = DoubleDom#DomVar
  type VectorVar = VectorDom#DomVar
  type MatrixVar = MatrixDom#DomVar
  type DiscVar[T] = GenericDiscreteDom[T]#DomVar
  type Constant[D <: Dom] = D#Constant
  type AnyVar = Var[Dom]
  type AnyTerm = Term[Dom]
  type AnyAtom = Atom[Dom]
  type AnyGroundAtom = GroundAtom[Dom]
  type AnySeqDom = VarSeqDom[Dom]
  type Mem = Memoized[Dom, AnyTerm]
  type SampleTerm = RangeDom#SampleTerm
  type VarSeqTerm = VarSeqDom[Dom]#Term

  type TypedDom[T] = Dom { type Value = T}
  type TypedTerm[T] = Term[TypedDom[T]]
  type IntTerm = Term[RangeDom] //TypedTerm[Int]
  type IntDom = RangeDom
  type DomWithTerm[T] = Dom { type Term = T}
  type DoubleTerm = TypedTerm[Double]
  type VectorTerm = Term[GenericVectorDom]
  type MatrixTerm = Term[MatrixDom]
  type BoolDom = TypedDom[Boolean]
  type BoolTerm = TypedTerm[Boolean]
  type DiscreteTerm[T] = Term[GenericDiscreteDom[T]]
//  type IntTerm = DiscreteTerm[Int]

}
