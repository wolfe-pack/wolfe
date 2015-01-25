package ml.wolfe

import ml.wolfe.fg20.{Domain, IndexedSeqDomain, AtomicDomain}

/**
 * @author riedel
 */
package object term {
  type Potential2[D1<: Domain[_], D2 <:Domain[_] ] = Potential[Tuple2Domain[_,_,D1,D2]]
  type DoubleSeqDom = IndexedSeqDomain[Double,AtomicDomain.Cont]
//  type Potential = Term[Double,AtomicSearchSpace.Cont]
//  type ComposedPotential = Composed[Double,AtomicSearchSpace.Cont]

}
