package ml.wolfe.term

import cc.factorie.la.DenseTensor1
import ml.wolfe._
import ml.wolfe.fg20.Msgs


trait Dom {
  type Value
  type TermType <: Term[Dom]
  type Variable <: Var[Dom] with TermType

  def toValue(setting: Setting, offsets: Offsets = Offsets()): Value
  def copyValue(value: Value, setting: Setting, offsets: Offsets = Offsets())

  def toSetting(value: Value): Setting = {
    val result = createSetting()
    copyValue(value, result)
    result
  }
  def createSetting(): Setting = new Setting(lengths.discOff, lengths.contOff, lengths.vectOff)
  def createMsgs() = new Msgs(null, null, null)
  def createZeroSetting(): Setting = {
    val result = createSetting()
    copyValue(zero, result)
    result
  }
  def variable(name: String, offsets: Offsets = Offsets(), owner: Var[Dom] = null): Variable

  def lengths: Offsets

  def isDiscrete = lengths.contOff == 0 && lengths.vectOff == 0
  def isContinuous = lengths.discOff == 0
  def isDouble = lengths.contOff == 1 && lengths.discOff == 0 && lengths.vectOff == 0


  def one: Value
  def zero: Value


}

object Dom {
  val doubles = new DoubleDom
  val bools   = TermImplicits.discrete(false, true)
}

class VectorDom(val dim: Int) extends Dom {
  type Value = FactorieVector
  type Variable = VectorVar
  type TermType = VectorTerm

  val lengths = Offsets(0, 0, 1)

  def toValue(setting: Setting, offsets: Offsets) =
    setting.vect(offsets.vectOff)
  def copyValue(value: Value, setting: Setting, offsets: Offsets) =
    setting.vect(offsets.vectOff) = value
  def variable(name: String, offsets: Offsets, owner: Var[Dom]) = VectorVar(name, owner, this, offsets.vectOff)
  def one = new DenseTensor1(dim, 1.0)
  def zero = new DenseTensor1(dim, 0.0)
}

class DoubleDom extends Dom {
  type Value = Double
  type TermType = DoubleTerm
  def toValue(setting: Setting, offsets: Offsets = Offsets()) =
    setting.cont(offsets.contOff)
  def copyValue(value: Value, setting: Setting, offsets: Offsets = Offsets()) =
    setting.cont(offsets.contOff) = value
  val lengths = Offsets(0, 1, 0)
  type Variable = DoubleVar
  def variable(name: String, offsets: Offsets = Offsets(), owner: Var[Dom]) = DoubleVar(name, owner, this, offsets.contOff)
  def one = 1.0
  def zero = 0.0
}

class DiscreteDom[T](val values: IndexedSeq[T]) extends Dom {
  type Value = T
  type TermType = Term[DiscreteDom[T]]
  def toValue(setting: Setting, offsets: Offsets = Offsets()) =
    values(setting.disc(offsets.discOff))
  def copyValue(value: Value, setting: Setting, offsets: Offsets = Offsets()) =
    setting.disc(offsets.discOff) = values.indexOf(value)
  val lengths = Offsets(1, 0, 0)
  type Variable = DiscVar[T]
  def variable(name: String, offsets: Offsets = Offsets(), owner: Var[Dom]) = DiscVar(name, owner, this, offsets.discOff)
  def one = values.last
  def zero = values.head
  def const(value:T) = new Constant[DiscreteDom[T]](this,value)

}



class SeqDom[D <: Dom](val elementDom: D, val length: Int) extends Dom {

  type Value = IndexedSeq[elementDom.Value]
  type TermType = SeqTerm[D]
  type Variable = SeqVar[D]

  def toValue(setting: Setting, offsets: Offsets = Offsets()) = {
    val result = for (i <- 0 until length) yield elementDom.toValue(setting, offsets +(elementDom.lengths, i)
    )
    result
  }
  def copyValue(value: Value, setting: Setting, offsets: Offsets = Offsets()) = {
    for (i <- 0 until length) {
      elementDom.copyValue(value(i), setting, offsets +(elementDom.lengths, i))
    }
  }
  val lengths = elementDom.lengths * length
  def variable(name: String, offsets: Offsets = Offsets(), owner: Var[Dom]) = SeqVar(name, this, offsets, owner)
  def one = for (i <- 0 until length) yield elementDom.one
  def zero = for (i <- 0 until length) yield elementDom.zero

}

class Tuple2Dom[D1 <: Dom, D2 <: Dom](val dom1: D1, val dom2: D2) extends Dom {
  type Value = (dom1.Value, dom2.Value)
  type Variable = Tuple2Var[D1, D2]
  type TermType = Tuple2Term[D1,D2]
  val lengths = dom1.lengths + dom2.lengths
  def toValue(setting: Setting, offsets: Offsets = Offsets()) = {
    val arg1 = dom1.toValue(setting, offsets)
    val arg2 = dom2.toValue(setting, offsets + dom1.lengths)
    (arg1, arg2)
  }
  def copyValue(value: Value, setting: Setting, offsets: Offsets = Offsets()): Unit = {
    dom1.copyValue(value._1, setting)
    dom2.copyValue(value._2, setting, dom1.lengths)
  }
  def variable(name: String, offsets: Offsets, owner: Var[Dom]) =
    Tuple2Var(name, this, offsets, owner)
  def one = (dom1.one, dom2.one)
  def zero = (dom1.zero, dom2.zero)
}
