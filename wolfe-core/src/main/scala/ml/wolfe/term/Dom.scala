package ml.wolfe.term

import cc.factorie.la.{DenseTensor1, DenseTensor2}
import ml.wolfe._
import ml.wolfe.term.ExhaustiveSearch.AllSettings

import scala.collection.mutable.ArrayBuffer


trait Dom {

  dom =>

  type Value
  type Var <: DomVar with Term
  type Term <: DomTerm
  type Marginals

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
  def dynamic(name: => String, offsets: => Offsets = Offsets(), owner: term.Var[Dom] = null): Var
  def const(value: Value): Term

  def lengths: Offsets

  def isDiscrete = lengths.contOff == 0 && lengths.vectOff == 0 && lengths.matsOff == 0
  def isContinuous = lengths.discOff == 0
  def isDouble = lengths.contOff == 1 && lengths.discOff == 0 && lengths.vectOff == 0 && lengths.matsOff == 0


  def one: Value
  def zero: Value

  abstract class BaseVar(dynName: => String, val owner: term.Var[Dom]) extends DomVar {
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

    def atoms = Atoms()

    def differentiator(wrt: Seq[term.Var[Dom]]) = new Differentiator {
      val result = domain.toSetting(value.asInstanceOf[domain.Value])

      def forwardProp(current: Array[Setting]) = activation := result

      def term = self

      def withRespectTo = wrt

      def backProp(error: Setting, gradient: Array[Setting]) = {}
    }
  }

  def iterator:Iterator[Value] = {
    require(isDiscrete, "domain must be discrete to iterate over all states")
    val tmp = variable("tmp")
    val settingToVary = createSetting()
    val disc = tmp.atoms.disc.toArray
    val loop = new AllSettings(Array(disc))
    val result = new ArrayBuffer[Value]()
    //todo: do a version that doesn't need to store the list in advance
    loop.iterate(Array(settingToVary)) {
      result += toValue(settingToVary)
    }
    result.iterator
  }


}

object Dom {
  val doubles = new DoubleDom
  val bools   = TermImplicits.discrete(false, true)

  def createSettings(vars:Seq[Var[Dom]],values:Seq[Any]):Array[Setting] = {
    (for ((a, v) <- values zip vars) yield v.domain.toSetting(a.asInstanceOf[v.domain.Value])).toArray
  }
}






