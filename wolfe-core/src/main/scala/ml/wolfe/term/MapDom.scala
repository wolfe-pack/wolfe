package ml.wolfe.term

/**
 * @author riedel
 */
abstract class MapDom[K <: Dom, V <: Dom](val keyDom: K, val valueDom: V) extends Dom {

  dom =>

  import ml.wolfe.term.TermImplicits._

  require(keyDom.isDiscrete, "keys of map need to be discrete")

  val optionDom = Bools x valueDom
  val seqDom = Seqs(optionDom, keyDom.domainSize)

  type Value = Map[keyDom.Value, valueDom.Value]
  type Marginals = Map[keyDom.Value, (Bools.Marginals,valueDom.Marginals)]
  type Term <: DomTerm
  type Var <: Term with DomVar

  def seq2map(seq: seqDom.Value) = {
    val map = for (((exists, value), index) <- seq.zipWithIndex; if exists) yield keyDom.indexToValue(index) -> value
    map.toMap
  }

  def map2seq(map: Value) = {
    for (i <- 0 until keyDom.domainSize) yield map.get(keyDom.indexToValue(i)) match {
      case Some(v) => (true, v)
      case _ => (false, valueDom.zero)
    }
  }

  trait DomTerm extends super.DomTerm {

    def apply(key: keyDom.Term): valueDom.Term
  }

  class SeqWrapper(val seqTerm: seqDom.Term) extends DomTerm with ProxyTerm[Dom] {
    def self = seqTerm

    override val domain: dom.type = dom

    def apply(key: keyDom.Term): valueDom.Term = seqTerm(IndexOf(key))._2


    def copy(args: IndexedSeq[ArgumentType]) = new SeqWrapper(args(0).asInstanceOf[seqDom.Term])
  }

  def toValue(setting: Setting, offsets: Offsets) = {
    val seq = seqDom.toValue(setting, offsets)
    //todo:this bit may be slow as we always iterate over all keys
    seq2map(seq)
  }

  class DomVar(val name: String) extends DomTerm with super.DomVar {
    def apply(key: keyDom.Term) = seqDom.own(this.asInstanceOf[TypedTerm[seqDom.Value]])(IndexOf(key))._2
  }



  def fillZeroMsg(target: Msg, offsets: Offsets) = seqDom.fillZeroMsg(target, offsets)


  def toMarginals(msg: Msg, offsets: Offsets) = {
    val seqMargs = seqDom.toMarginals(msg, offsets)
    val map = for (((exists, valueMarg), index) <- seqMargs.elements zipWithIndex) yield
      keyDom.indexToValue(index) -> (exists,valueMarg)
    map.toMap
  }


  def mapMarg2seqMarg(map: Marginals):seqDom.Marginals = {
    val elems = for (i <- 0 until keyDom.domainSize) yield map.get(keyDom.indexToValue(i)) match {
      case Some(p) => p
      case _ => (Map(false -> 1.0, true -> 0.0), valueDom.toMarginals(valueDom.createZeroMsg()))
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

  def zero = Map.empty

  def one = keyDom.toIterable.map(k => k -> valueDom.one).toMap

}

class MapDom1[K <: Dom, V <: Dom](keyDom: K, valueDom: V) extends MapDom[K,V](keyDom,valueDom) {

  type Term = DomTerm
  type Var = DomVar

  def variable(varName: String) = new Var(varName)

  def Const(value: Value) = new SeqWrapper(seqDom.Const(map2seq(value)))

  //trait Test extends Term
  def own(term: TypedTerm[Value]) = new SeqWrapper(seqDom.own(term.asInstanceOf[TypedTerm[seqDom.Value]]))

}

class MapDom2[K1 <: Dom, K2 <: Dom, V <:Dom](val keyDom1:K1, val keyDom2:K2, override val valueDom:V)
  extends MapDom[K1 x K2,V](new Tuple2Dom[K1,K2](keyDom1,keyDom2),valueDom) {

  trait Term extends DomTerm {
    def apply(key1: keyDom1.Term,key2:keyDom2.Term):valueDom.Term = {
      val key = keyDom.Term(key1.asInstanceOf[keyDom.dom1.Term],key2.asInstanceOf[keyDom.dom2.Term])
      apply(key)
    }
  }

  class Var(name:String) extends DomVar(name) with Term

  def variable(varName: String) = new Var(varName)

  def Const(value: Value) = new SeqWrapper(seqDom.Const(map2seq(value))) with Term

  //trait Test extends Term
  def own(term: TypedTerm[Value]) = new SeqWrapper(seqDom.own(term.asInstanceOf[TypedTerm[seqDom.Value]])) with Term

}

case class IndexOf[D <: Dom](term: Term[D]) extends Composed[RangeDom] {
  val domain = new RangeDom(0 until term.domain.domainSize)
  type ArgumentType = Term[D]

  val arguments = IndexedSeq(term)

  def copy(args: IndexedSeq[ArgumentType]) = IndexOf(args(0))

  override def composer(args: Settings) = new Composer(args) {
    def eval()(implicit execution: Execution) = {
      output.disc(0) = term.domain.indexOfSetting(input(0))
    }
  }
}
