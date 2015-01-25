package ml.wolfe

import ml.wolfe.fg20.{Domain, IndexedSeqDomain, AtomicDomain}

/**
 * @author riedel
 */
package object term {
  type Potential2[D1<: Dom, D2 <:Dom] = Potential[Tuple2Domain[D1,D2]]
  type DoubleSeqDom = SeqDom[DoubleDom]
//  type Potential[P] = Fun[P,DoubleDom]
//  type ComposedPotential = Composed[Double,AtomicSearchSpace.Cont]

}
