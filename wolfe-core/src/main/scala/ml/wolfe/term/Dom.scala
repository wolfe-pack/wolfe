package ml.wolfe.term

import ml.wolfe._


/**
 * A domain defines a set of possible values. Examples are the set of booleans, doubles, general discrete domains,
 * but also "structured domains" such as the set of all sequences of booleans of length 5.
 */
trait Dom {

  dom =>

  /**
   * The type of value this domain contains.
   */
  type Value

  /**
   * The type of variables of this domain.
   */
  type Var <: term.Var[dom.type] with Term

  /**
   * The type of terms this domain provides.
   */
  type Term <: term.Term[dom.type]

  /**
   * How marginals are represented for values of this domain.
   */
  type Marginals

  /**
   * A type to define terms of sequences of elements of this domain.
   */
  type SeqTerm = VarSeqDom[dom.type]#Term

  /**
   * This takes a term and makes it a term of this domain. Use this with care, it will only work when
   * the term's setting based representation is compatible with this domain's setting based representation.
   * @param term the term to be owned by the domain.
   * @return a term with the same semantics, but owned by this domain.
   */
  def own(term: TypedTerm[Value]): Term


  trait DomTerm extends term.Term[dom.type] {
    val domain: dom.type = dom
  }

  trait DomVar extends term.Var[dom.type] with DomTerm

  def toValue(setting: Setting, offsets: Offsets = Offsets()): Value

  def toMarginals(msg: Msg, offsets: Offsets = Offsets()): Marginals

  def copyMarginals(marginals: Marginals, msgs: Msg, offsets: Offsets = Offsets())

  def copyValue(value: Value, setting: Setting, offsets: Offsets = Offsets())

  def x(that: Dom) = new Tuple2Dom[dom.type, that.type](dom, that)

  def ==>(that:Dom) = new MapDom1[dom.type,that.type](dom,that)

  def toSetting(value: Value): Setting = {
    val result = createSetting()
    copyValue(value, result)
    result
  }

  def createRandomSetting(eps: => Double) = {
    toSetting(zero).randomize(eps)
  }

  def toMsgs(marginals: Marginals): Msg = {
    val result = createMsg()
    copyMarginals(marginals, result)
    result
  }

  def createSetting(): Setting = new Setting(lengths.discOff, lengths.contOff, lengths.vectOff, lengths.matsOff)

  def createMsg() = new Msg(lengths.discOff, lengths.contOff, lengths.vectOff, lengths.matsOff)

  def createZeroMsg() = {
    val result = createMsg()
    fillZeroMsg(result)
    result
  }

  def fillZeroMsg(target: Msg, offsets: Offsets = Offsets())

  def createZeroSetting(): Setting = {
    val result = createSetting()
    copyValue(zero, result)
    result
  }

  def createSparseZeroSetting(): Setting = {
    val result = createSetting()
    copyValue(sparseZero, result)
    result
  }


  def Variable(name: String): Var

  def Var(implicit provider: NameProvider): Var = Variable(provider.newName())

  def Const(value: Value): Term

  def lengths: Offsets

  def dimensions: Dimensions = Dimensions()

  lazy val domainSize = {
    require(isDiscrete,"domain size is only defined for discrete domains")
    dimensions.discDims.map(_.size).product
  }

  def indexOfSetting(setting: Setting): Int = {
    require(isDiscrete, "only discrete domains have indices")
    var result = 0
    for (i <- 0 until setting.disc.length) {
      val index = setting.disc(i) - dimensions.discDims(i).start
      result = index + result * dimensions.discDims(i).size
    }
    result
  }

  def indexToValue(index:Int) = {
    val setting = createSetting()
    settingOfIndex(index,setting)
    toValue(setting)
  }

  def settingOfIndex(index: Int, tgt: Setting): Unit = {
    val result = tgt.disc
    var current = index
    def dim(i: Int) = dimensions.discDims(i).size
    def length = dimensions.discDims.length
    for (i <- 0 until length) {
      // .optimized) {
      val value = current % dim(length - i - 1)
      result(length - i - 1) = value
      current = current / dim(length - i - 1)
    }
    /*
    val result = Array.ofDim[Int](dims.length)
    var current = entry
    for (i <- (0 until dims.length)) { // .optimized) {
      val value = current % dims(dims.length - i - 1)
      result(dims.length - i - 1) = value
      current = current / dims(dims.length - i - 1)
    }
    result

       */
  }


  def isDiscrete = lengths.contOff == 0 && lengths.vectOff == 0 && lengths.matsOff == 0

  def isContinuous = lengths.discOff == 0

  def isDouble = lengths.contOff == 1 && lengths.discOff == 0 && lengths.vectOff == 0 && lengths.matsOff == 0

  def one: Value

  def zero: Value

  def sparseZero = zero

  abstract class BaseVar(val varName: String) extends DomVar {
  }

  case class Constant(value: Value) extends DomTerm {
    self =>

    //def isStatic = true

    val vars = Seq.empty

    override def evaluatorImpl(in: Settings) = new AbstractEvaluator(in) {
      def eval()(implicit execution: Execution) {}

      val output = domain.toSetting(value.asInstanceOf[domain.Value])
    }

    override def toString = value.toString
  }


  import scala.language.implicitConversions

  object conversion {
    implicit def toConst(value: dom.Value): dom.Term = Const(value)
  }

  def toIterable: Iterable[Value] = {
    require(isDiscrete, "domain must be discrete to iterate over all states")
    def settingToVary = Settings(createSetting())
    new AllSettings[Value](IndexedSeq(this), settingToVary)(s => toValue(s(0)))
  }

}

object Dom {
  val doubles = new DoubleDom
  val bools = new BooleanDom
  val ints = new RangeDom(Int.MinValue until Int.MaxValue)

  def createSettingsArray(vars: Seq[Var[Dom]], values: Seq[Any]): Array[Setting] = {
    (for ((a, v) <- values zip vars) yield v.domain.toSetting(a.asInstanceOf[v.domain.Value])).toArray
  }

  def createSettings(vars: Seq[Var[Dom]], values: Seq[Any]): Settings = {
    Settings.fromSeq(for ((a, v) <- values zip vars) yield v.domain.toSetting(a.asInstanceOf[v.domain.Value]))
  }

}






