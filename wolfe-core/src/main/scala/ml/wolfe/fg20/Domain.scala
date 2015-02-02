package ml.wolfe.fg20

import ml.wolfe.FactorieVector
import ml.wolfe.term.Setting

/**
 * A search space describes a set of possible values using a set of variables. Any setting to the search space's
 * variables can be mapped to a value in the search space. Likewise, any value in the search space can be mapped
 * to an assignment of variables.
 *
 * @author Sebastian Riedel
 */
trait Domain[T] extends Clique {
  type Value = T
  def variables: Iterator[Var[Any]]
  def toValue(state: State): T
  def toValue(setting:Setting):Value = toValue(toState(setting))
  def toState(value: Value): State

  def discVars = variables.collect({ case d: DiscVar[_] => d }).toArray
  def contVars = variables.collect({ case c: ContVar => c }).toArray
  def vectVars = variables.collect({ case v: VectVar => v }).toArray

  def createPartialSetting(state:Value):PartialSetting = {
    createPartialSetting(toState(state))
  }


}

object AtomicDomain {
  type Disc[T] = AtomicDomain[T, DiscVar[T]]
  type Cont = AtomicDomain[Double, ContVar]
  type Vect = AtomicDomain[FactorieVector, VectVar]

  def disc[T](name:String,dom:IndexedSeq[T]) = new Disc[T](new DiscVar[T](dom,name))
  def cont(name:String) = new Cont(new ContVar(name))
  def vect(name:String, dim:Int = -1) = new Vect(new VectVar(dim, name))

  def constDisc[T](value:T) = new Disc[T](new DiscVar[T](IndexedSeq(value)))

}

class AtomicDomain[T, V <: Var[T]](val variable: V) extends Domain[T] {
  def toValue(state: State) = state(variable)
  def toState(value: T) = State.single(variable, value)
  def variables = Iterator(variable)

}

object IndexedSeqDomain {
  type Disc[T] = IndexedSeqDomain[T, AtomicDomain.Disc[T]]
  type Vect = IndexedSeqDomain[FactorieVector, AtomicDomain.Vect]



}

class IndexedSeqDomain[T, S <: Domain[T]](val size: Int,
                                                    elementSpace: String => S,
                                                    val name: String = "anon") extends Domain[IndexedSeq[T]] {
  lazy val seq = Range(0, size).map(i => elementSpace(name + "(" + i + ")"))
  def toValue(state: State) = seq.map(_.toValue(state))
  def toState(value: IndexedSeq[T]) = ???
  def variables = seq.iterator.flatMap(_.variables)

}

object GraphDomain {
  type Disc[T] = GraphDomain[T, AtomicDomain.Disc[T]]
  type Cont = GraphDomain[Double, AtomicDomain.Cont]
}

class GraphDomain[V, S <: Domain[V]](val sources: Range,
                                               val targets: Range,
                                               valueSpace: String => S,
                                               val name: String = "anon") extends Domain[Map[(Int, Int), V]] {
  val sourceCount = sources.size
  val targetCount = sources.size
  def variables = ???
  def toState(value: Map[(Int, Int), V]) = ???
  def toValue(state: State) = ???
  final def offset(src: Int, tgt: Int) = src * sources.size + tgt
}

object MapDomain {
  type Disc[K, T] = MapDomain[K, T, AtomicDomain.Disc[T]]
  type Cont[K] = MapDomain[K, Double, AtomicDomain.Cont]
}

class MapDomain[K, V, VS <: Domain[V]](keys: Iterable[K],
                                                 valueSpace: String => VS,
                                                 val name: String = "anon") extends Domain[Map[K, V]] {
  lazy val map = keys.map(k => k -> valueSpace(name + "(" + k + ")")).toMap
  def toValue(state: State) = map.mapValues(_.toValue(state))
  def toState(value: Map[K, V]) = {
    val result = new MutableState
    for ((k, v) <- map) {
      val state = v.toState(value(k))
      result ++= state
    }
    result
  }
  def variables = map.valuesIterator.flatMap(_.variables)

}

class ProductDomain2[T1, T2, S1 <: Domain[T1], S2 <: Domain[T2], P](val space1: S1,
                                                                                   val space2: S2,
                                                                                   val ctr: (T1, T2) => P,
                                                                                   val arg1: P => T1,
                                                                                   val arg2: P => T2) extends Domain[P] {
  def toValue(state: State) = ctr(space1.toValue(state), space2.toValue(state))
  def toState(value: P) = space1.toState(arg1(value)) + space2.toState(arg2(value))
  def variables = space1.variables ++ space2.variables
}

object ProductDomain2 {

  def apply[T1,T2,S1 <: Domain[T1], S2 <: Domain[T2]](space1: S1,space2: S2) =
    new ProductDomain2[T1,T2,S1,S2,(T1,T2)](space1,space2, (x1:T1,x2:T2) => (x1,x2), _._1, _._2)
}

