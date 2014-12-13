package ml.wolfe.fg20

import ml.wolfe.FactorieVector

/**
 * A search space describes a set of possible values using a set of variables. Any setting to the search space's
 * variables can be mapped to a value in the search space. Likewise, any value in the search space can be mapped
 * to an assignment of variables.
 *
 * @author Sebastian Riedel
 */
trait SearchSpace[T] extends Clique {
  def variables: Iterator[Var[Any]]
  def toValue(state: State): T
  def observation(value: T): State

  def discVars = variables.collect({ case d: DiscVar[_] => d }).toArray
  def contVars = variables.collect({ case c: ContVar => c }).toArray
  def vectVars = variables.collect({ case v: VectVar => v }).toArray

}

object AtomicSearchSpace {
  type Disc[T] = AtomicSearchSpace[T, DiscVar[T]]
  type Cont = AtomicSearchSpace[Double, ContVar]
  type Vect = AtomicSearchSpace[FactorieVector, VectVar]

  def constDisc[T](value:T) = new Disc[T](new DiscVar[T](Seq(value)))

}

class AtomicSearchSpace[T, V <: Var[T]](val variable: V) extends SearchSpace[T] {
  def toValue(state: State) = state(variable)
  def observation(value: T) = State.single(variable, value)
  def variables = Iterator(variable)

}

object IndexedSeqSearchSpace {
  type Disc[T] = IndexedSeqSearchSpace[T, AtomicSearchSpace.Disc[T]]
  type Vect = IndexedSeqSearchSpace[FactorieVector, AtomicSearchSpace.Vect]

}

class IndexedSeqSearchSpace[T, S <: SearchSpace[T]](val size: Int,
                                                    elementSpace: String => S,
                                                    val name: String = "anon") extends SearchSpace[IndexedSeq[T]] {
  lazy val seq = Range(0, size).map(i => elementSpace(name + "(" + i + ")"))
  def toValue(state: State) = seq.map(_.toValue(state))
  def observation(value: IndexedSeq[T]) = ???
  def variables = seq.iterator.flatMap(_.variables)

}

object GraphSearchSpace {
  type Disc[T] = GraphSearchSpace[T, AtomicSearchSpace.Disc[T]]
  type Cont = GraphSearchSpace[Double, AtomicSearchSpace.Cont]
}

class GraphSearchSpace[V, S <: SearchSpace[V]](val sources: Range,
                                               val targets: Range,
                                               valueSpace: String => S,
                                               val name: String = "anon") extends SearchSpace[Map[(Int, Int), V]] {
  val sourceCount = sources.size
  val targetCount = sources.size
  def variables = ???
  def observation(value: Map[(Int, Int), V]) = ???
  def toValue(state: State) = ???
  final def offset(src: Int, tgt: Int) = src * sources.size + tgt
}

object MapSearchSpace {
  type Disc[K, T] = MapSearchSpace[K, T, AtomicSearchSpace.Disc[T]]
  type Cont[K] = MapSearchSpace[K, Double, AtomicSearchSpace.Cont]
}

class MapSearchSpace[K, V, VS <: SearchSpace[V]](keys: Iterable[K],
                                                 valueSpace: String => VS,
                                                 val name: String = "anon") extends SearchSpace[Map[K, V]] {
  lazy val map = keys.map(k => k -> valueSpace(name + "(" + k + ")")).toMap
  def toValue(state: State) = map.mapValues(_.toValue(state))
  def observation(value: Map[K, V]) = {
    val result = new MutableState
    for ((k, v) <- map) {
      val state = v.observation(value(k))
      result ++= state
    }
    result
  }
  def variables = map.valuesIterator.flatMap(_.variables)

}

class ProductSearchSpace2[T1, T2, S1 <: SearchSpace[T1], S2 <: SearchSpace[T2], P](val space1: S1,
                                                                                   val space2: S2,
                                                                                   val ctr: (T1, T2) => P,
                                                                                   val arg1: P => T1,
                                                                                   val arg2: P => T2) extends SearchSpace[P] {
  def toValue(state: State) = ctr(space1.toValue(state), space2.toValue(state))
  def observation(value: P) = space1.observation(arg1(value)) + space2.observation(arg2(value))
  def variables = space1.variables ++ space2.variables
}


