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

  //trait Test extends Term
  def own(term:TypedTerm[Value]):Term

  trait DomTerm extends term.Term[dom.type] {
    val domain: dom.type = dom
  }

  trait DomVar extends term.Var[dom.type] with DomTerm

  def toValue(setting: Setting, offsets: Offsets = Offsets()): Value
  def toMarginals(msg: Msg, offsets: Offsets = Offsets()): Marginals
  def copyMarginals(marginals:Marginals, msgs:Msg,offsets: Offsets = Offsets())
  def copyValue(value: Value, setting: Setting, offsets: Offsets = Offsets())

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
  def fillZeroMsg(target:Msg,offsets: Offsets = Offsets())
  def createZeroSetting(): Setting = {
    val result = createSetting()
    copyValue(zero, result)
    result
  }
  def variable(name: String): Var

  def Var(implicit provider:NameProvider):Var = variable(provider.newName())

  def Const(value: Value): Term

  def lengths: Offsets
  def dimensions: Dimensions = Dimensions()

  def isDiscrete = lengths.contOff == 0 && lengths.vectOff == 0 && lengths.matsOff == 0
  def isContinuous = lengths.discOff == 0
  def isDouble = lengths.contOff == 1 && lengths.discOff == 0 && lengths.vectOff == 0 && lengths.matsOff == 0

  def one: Value
  def zero: Value

  abstract class BaseVar(val name: String) extends DomVar {
  }

  case class Constant(value: Value) extends DomTerm {
    self =>

    def isStatic = true

    val vars      = Seq.empty

    override def evaluatorImpl(in: Settings) = new AbstractEvaluator(in) {
      def eval()(implicit execution: Execution) {}
      val output = domain.toSetting(value.asInstanceOf[domain.Value])
    }

    override def toString = value.toString
  }



  import scala.language.implicitConversions

  object conversion {
    implicit def toConst(value:dom.Value): dom.Term = Const(value)
  }

  def toIterable:Iterable[Value] = {
    require(isDiscrete, "domain must be discrete to iterate over all states")
    def settingToVary = Settings(createSetting())
    new AllSettings[Value](IndexedSeq(this),settingToVary)(s => toValue(s(0)))
  }

}

object Dom {
  val doubles = new DoubleDom
  val bools   = new BooleanDom
  val ints   = new RangeDom(Int.MinValue until Int.MaxValue)

  def createSettingsArray(vars:Seq[Var[Dom]],values:Seq[Any]):Array[Setting] = {
    (for ((a, v) <- values zip vars) yield v.domain.toSetting(a.asInstanceOf[v.domain.Value])).toArray
  }
  def createSettings(vars:Seq[Var[Dom]],values:Seq[Any]):Settings = {
    Settings.fromSeq(for ((a, v) <- values zip vars) yield v.domain.toSetting(a.asInstanceOf[v.domain.Value]))
  }

}






