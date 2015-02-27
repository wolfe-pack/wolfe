package ml.wolfe.term

import cc.factorie.la.{DenseTensor1, DenseTensor2}
import ml.wolfe._
import ml.wolfe.term.ExhaustiveSearch.{AllSettingsIterable, AllSettings}

import scala.collection.mutable.ArrayBuffer


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
  def toMarginals(msg: Msgs, offsets: Offsets = Offsets()): Marginals
  def copyMarginals(marginals:Marginals, msgs:Msgs,offsets: Offsets = Offsets())
  def copyValue(value: Value, setting: Setting, offsets: Offsets = Offsets())

  def toSetting(value: Value): Setting = {
    val result = createSetting()
    copyValue(value, result)
    result
  }

  def toMsgs(marginals: Marginals): Msgs = {
    val result = createMsgs()
    copyMarginals(marginals, result)
    result
  }

  def createSetting(): Setting = new Setting(lengths.discOff, lengths.contOff, lengths.vectOff, lengths.matsOff)
  def createMsgs() = new Msgs(lengths.discOff, lengths.contOff, lengths.vectOff, lengths.matsOff)
  def createZeroMsgs() = {
    val result = createMsgs()
    fillZeroMsgs(result)
    result
  }
  def fillZeroMsgs(target:Msgs,offsets: Offsets = Offsets())
  def createZeroSetting(): Setting = {
    val result = createSetting()
    copyValue(zero, result)
    result
  }
  def variable(name: String, offsets: Offsets = Offsets(), owner: term.Var[Dom] = null): Var

  def Var(implicit provider:NameProvider):Var = variable(provider.newName())

  def vari(implicit nameProvider: NameProvider):Var = variable(nameProvider.newName())

  def dynamic(name: => String, offsets: => Offsets = Offsets(), owner: term.Var[Dom] = null): Var
  def const(value: Value): Term

  def lengths: Offsets

  def isDiscrete = lengths.contOff == 0 && lengths.vectOff == 0 && lengths.matsOff == 0
  def isContinuous = lengths.discOff == 0
  def isDouble = lengths.contOff == 1 && lengths.discOff == 0 && lengths.vectOff == 0 && lengths.matsOff == 0

  def one: Value
  def zero: Value

  abstract class BaseVar(dynName: => String, val owner: term.Var[Dom] = null) extends DomVar {
    def name = dynName
  }

  class Constant(val value: Value) extends DomTerm {
    self =>

    val vars      = Seq.empty
    val evaluator = new Evaluator {
      val result = domain.toSetting(value.asInstanceOf[domain.Value])

      def eval(inputs: Array[Setting], output: Setting) = {
        output := result
      }
    }

    def atomsIterator = Iterator.empty

    def differentiator(wrt: Seq[term.Var[Dom]]) = new Differentiator {
      val result = domain.toSetting(value.asInstanceOf[domain.Value])

      def forwardProp(current: Array[Setting]) = activation := result

      def term = self

      def withRespectTo = wrt

      def backProp(error: Setting, gradient: Array[Setting]) = {}
    }
  }

  class DynamicConstant(val value: Dynamic[Value]) extends DomTerm {
    self =>

    val vars      = Seq.empty
    val evaluator = new Evaluator {

      def eval(inputs: Array[Setting], output: Setting) = {
        domain.copyValue(value.value(),output)
      }
    }

    def atomsIterator = Iterator.empty

    def differentiator(wrt: Seq[term.Var[Dom]]) = new Differentiator {

      def forwardProp(current: Array[Setting]) = {
        domain.copyValue(value.value(),activation)
      }

      def term = self

      def withRespectTo = wrt

      def backProp(error: Setting, gradient: Array[Setting]) = {}
    }
  }


  import scala.language.implicitConversions

  object conversion {
    implicit def toConst(value:dom.Value): dom.Term = const(value)
  }

  def toIterable:Iterable[Value] = {
    require(isDiscrete, "domain must be discrete to iterate over all states")
    val settingToVary = createSetting()
    val tmp = variable("tmp")
    val atoms = tmp.atoms.disc.map(a => ExhaustiveSearch.IndexedAtom(a,0)).toIndexedSeq
    val iterable = new AllSettingsIterable[Value](atoms,Array(settingToVary),a => toValue(a(0)))
    iterable
  }

}

object Dom {
  val doubles = new DoubleDom
  val bools   = TermImplicits.discrete(false, true)

  def createSettings(vars:Seq[Var[Dom]],values:Seq[Any]):Array[Setting] = {
    (for ((a, v) <- values zip vars) yield v.domain.toSetting(a.asInstanceOf[v.domain.Value])).toArray
  }
}






