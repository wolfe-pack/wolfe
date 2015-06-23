package ml.wolfe

/**
 * @author riedel
 */
package object term {
  type DoubleVar = DoubleDom#Var
  type VectorVar = VectorDom#Var
  type MatrixVar = MatrixDom#Var
  //type DiscVar[T] = GenericDiscreteDom[T]#DomVar
  type AnyVar = Var[Dom]
  type AnyTerm = Term[Dom]
  type AnyAtom = Atom[Dom]
  type AnyGroundAtom = GroundAtom[Dom]
  type AnySeqDom = VarSeqDom[Dom]
  type Mem = Memoized[Dom, AnyTerm]
  type SampleTerm = RangeDom#SampleTerm
  type AnySeqTerm = VarSeqDom[Dom]#Term
  type SeqTerm[D <: Dom] = VarSeqDom[D]#Term
  type MapTerm[K<:Dom,V<:Dom] = MapDom[K,V]#Term

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
//  type DiscreteTerm[T] = Term[GenericDiscreteDom[T]]
//  type IntTerm = DiscreteTerm[Int]

  type x[D1 <: Dom, D2 <:Dom] = Tuple2Dom[D1,D2]

//  type MapDom[K <: Dom, V <: Dom] = VarSeqDom[Tuple2Dom[Dom.bools.type,V]]
  type Pred[A] = Map[A, Boolean]


}
