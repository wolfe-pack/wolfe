package ml.wolfe.term

/**
 * @author sameer
 * @author riedel
 * @since 5/26/15.
 */
abstract class FullMapDom[K <: Dom, V <: Dom](val keyDom: K, val valueDom: V) extends Dom {

  dom =>

  import ml.wolfe.term.TermImplicits._

  require(keyDom.isDiscrete, "keys of map need to be discrete")

  val seqDom = Seqs(valueDom, keyDom.domainSize)

  type Value = Map[keyDom.Value, valueDom.Value]
  type Marginals = Map[keyDom.Value, valueDom.Marginals]
  type Term <: DomTerm
  type Var <: Term with DomVar

  def seq2map(seq: seqDom.Value) = {
    val map = for ((value, index) <- seq.zipWithIndex) yield keyDom.indexToValue(index) -> value
    map.toMap
  }

  def map2seq(map: Value) = {
    for (i <- 0 until keyDom.domainSize) yield map.get(keyDom.indexToValue(i)) match {
      case Some(v) => v
      case _ => throw new Error("key: " + keyDom.indexToValue(i) + " not found in " + map)
    }
  }

  trait DomTerm extends super.DomTerm {

    def apply(key: keyDom.Term): valueDom.Term
  }

  class SeqWrapper(val seqTerm: seqDom.Term) extends DomTerm with ProxyTerm[Dom] {
    def self = seqTerm

    override val domain: dom.type = dom

    def apply(key: keyDom.Term): valueDom.Term = seqTerm(key.idx)


    def copy(args: IndexedSeq[ArgumentType]) = new SeqWrapper(args(0).asInstanceOf[seqDom.Term])
  }

  def toValue(setting: Setting, offsets: Offsets) = {
    val seq = seqDom.toValue(setting, offsets)
    //todo:this bit may be slow as we always iterate over all keys
    seq2map(seq)
  }

  class DomVar(val varName: String) extends DomTerm with super.DomVar {
    def apply(key: keyDom.Term) = seqDom.own(this.asInstanceOf[TypedTerm[seqDom.Value]],keepAfterCleaning = true)(key.idx)
  }



  def fillZeroMsg(target: Msg, offsets: Offsets) = seqDom.fillZeroMsg(target, offsets)


  def toMarginals(msg: Msg, offsets: Offsets) = {
    val seqMargs = seqDom.toMarginals(msg, offsets)
    val map = for ((valueMarg, index) <- seqMargs.elements.zipWithIndex) yield
      keyDom.indexToValue(index) -> valueMarg
    map.toMap
  }


  def mapMarg2seqMarg(map: Marginals):seqDom.Marginals = {
    val elems = for (i <- 0 until keyDom.domainSize) yield map.get(keyDom.indexToValue(i)) match {
      case Some(p) => p
      case _ => throw new Error("key: " + keyDom.indexToValue(i) + " not found in " + map)
    }
    seqDom.Marginals(Map(keyDom.domainSize -> 1.0) withDefaultValue 0.0,elems)
  }

  def copyMarginals(marginals: Marginals, msgs: Msg, offsets: Offsets) = {
    seqDom.copyMarginals(mapMarg2seqMarg(marginals),msgs,offsets)
  }

  def copyValue(value: Value, setting: Setting, offsets: Offsets) = {
    seqDom.copyValue(map2seq(value), setting, offsets)
  }

  def lengths = seqDom.lengths
  override val dimensions = seqDom.dimensions

  def zero = Map.empty

  def one = keyDom.toIterable.map(k => k -> valueDom.one).toMap

  override def toIterable = super.toIterable.view.toList.distinct
}

class FullMapDom1[K <: Dom, V <: Dom](keyDom: K, valueDom: V) extends FullMapDom[K,V](keyDom,valueDom) {

  type Term = DomTerm
  type Var = DomVar

  def Variable(varName: String) = new Var(varName)

  def Const(value: Value) = new SeqWrapper(seqDom.Const(map2seq(value)))

  //trait Test extends Term
  def own(term: TypedTerm[Value]) =
    new SeqWrapper(seqDom.own(term.asInstanceOf[TypedTerm[seqDom.Value]],keepAfterCleaning = true))

}

class FullMapDom2[K1 <: Dom, K2 <: Dom, V <:Dom](val keyDom1:K1, val keyDom2:K2, override val valueDom:V)
  extends FullMapDom[K1 x K2,V](new Tuple2Dom[K1,K2](keyDom1,keyDom2),valueDom) {

  trait Term extends DomTerm {
    def apply(key1: keyDom1.Term,key2:keyDom2.Term):valueDom.Term = {
      val key = keyDom.Term(key1.asInstanceOf[keyDom.dom1.Term],key2.asInstanceOf[keyDom.dom2.Term])
      apply(key)
    }
  }

  class Var(varName:String) extends DomVar(varName) with Term

  def Variable(varName: String) = new Var(varName)

  def Const(value: Value) = new SeqWrapper(seqDom.Const(map2seq(value))) with Term

  //trait Test extends Term
  def own(term: TypedTerm[Value]) =
    new SeqWrapper(seqDom.own(term.asInstanceOf[TypedTerm[seqDom.Value]], keepAfterCleaning = true)) with Term

}
