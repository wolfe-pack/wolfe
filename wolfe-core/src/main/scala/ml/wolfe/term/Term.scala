package ml.wolfe.term

import cc.factorie.la.DenseTensor1
import ml.wolfe.FactorieVector
import ml.wolfe.fg20._
import System._

case class Offsets(discOff: Int = 0, contOff: Int = 0, vectOff: Int = 0) {
  def +(disc: Int, cont: Int, vect: Int) = Offsets(discOff + disc, contOff + cont, vectOff + vect)
  def +(that: Offsets, scale: Int = 1) =
    Offsets(discOff + scale * that.discOff, contOff + scale * that.contOff, vectOff + scale * that.vectOff)
  def *(scale:Int) = Offsets(scale * discOff, scale * contOff, scale * vectOff)
}
case class Ranges(from: Offsets, to: Offsets) {
  def copy(src: Setting, tgt: Setting): Unit = {
    arraycopy(src.disc, from.discOff, tgt.disc, 0, to.discOff - from.discOff)
    arraycopy(src.cont, from.contOff, tgt.cont, 0, to.contOff - from.contOff)
    arraycopy(src.vect, from.vectOff, tgt.vect, 0, to.vectOff - from.vectOff)

  }
}

trait Term[+D <: Dom] {
  val domain: D
  def evaluator(): Evaluator
  def vars: Seq[Var[Dom]]
  def apply(args: Seq[Any]): domain.Value = {
    val ev = evaluator()
    val output = domain.createSetting()
    val argSettings = for ((a, v) <- args zip vars) yield v.domain.toSetting(a.asInstanceOf[v.domain.Value])
    ev.eval(argSettings.toArray, output)
    domain.toValue(output)
  }
  def differentiator(variable: Var[Dom]): Differentiator
}

trait Var[+D <: Dom] extends Term[D] {
  def name:String
  def ranges: Ranges
  def owner: Var[Dom]
  def vars = ???
  def differentiator(variable: Var[Dom]) = ???
  def evaluator() = new Evaluator {
    val r = ranges
    def eval(inputs: Array[Setting], output: Setting) = {
      ranges.copy(inputs(0), output)
    }
  }
}


trait Dom {
  type Value
  type Variable <: Var[Dom]
  def toValue(setting: Setting, offsets: Offsets = Offsets()): Value
  def copyValue(value: Value, setting: Setting, offsets: Offsets = Offsets())

  def toSetting(value: Value): Setting = {
    val result = createSetting()
    copyValue(value, result)
    result
  }
  def createSetting(): Setting = new Setting(lengths.discOff, lengths.contOff, lengths.vectOff)
  def variable(name:String, offsets: Offsets = Offsets(), owner: Var[Dom] = null): Variable

  def lengths: Offsets

}

object Dom {
  val vectors = new VectorDom
  val doubles = new DoubleDom
}

class VectorDom extends Dom {
  type Value = FactorieVector
  type Variable = VectorVar

  val lengths = Offsets(0, 0, 1)

  def toValue(setting: Setting, offsets: Offsets) =
    setting.vect(offsets.vectOff)
  def copyValue(value: Value, setting: Setting, offsets: Offsets) =
    setting.vect(offsets.vectOff) = value
  def variable(name:String, offsets: Offsets, owner: Var[Dom]) = VectorVar(name, owner, this, offsets.vectOff)
}

case class VectorVar(name:String, owner: Var[Dom], domain: VectorDom, offset: Int) extends Var[VectorDom] {
  val ranges = Ranges(Offsets(0, 0, offset), Offsets(0, 0, offset + 1))
}
case class DoubleVar(name:String, owner: Var[Dom], domain: DoubleDom, offset: Int) extends Var[DoubleDom] {
  val ranges = Ranges(Offsets(0, offset, 0), Offsets(0, offset + 1, 0))
}

case class DiscVar[T](name:String, owner: Var[Dom], domain: DiscreteDom[T], offset: Int) extends Var[DiscreteDom[T]] {
  val ranges = Ranges(Offsets(offset, 0, 0), Offsets(offset + 1, 0, 0))
}


class DoubleDom extends Dom {
  type Value = Double
  def toValue(setting: Setting, offsets: Offsets = Offsets()) =
    setting.cont(offsets.contOff)
  def copyValue(value: Value, setting: Setting, offsets: Offsets = Offsets()) =
    setting.cont(offsets.contOff) = value
  val lengths = Offsets(0, 1, 0)
  type Variable = DoubleVar
  def variable(name:String, offsets: Offsets = Offsets(), owner: Var[Dom]) = DoubleVar(name, owner, this, offsets.contOff)
}

class DiscreteDom[T](val values: IndexedSeq[T]) extends Dom {
  type Value = T
  def toValue(setting: Setting, offsets: Offsets = Offsets()) =
    values(setting.disc(offsets.discOff))
  def copyValue(value: Value, setting: Setting, offsets: Offsets = Offsets()) =
    setting.disc(offsets.discOff) = values.indexOf(value)
  val lengths = Offsets(1, 0, 0)
  type Variable = DiscVar[T]
  def variable(name:String, offsets: Offsets = Offsets(), owner: Var[Dom]) = DiscVar(name, owner, this, offsets.discOff)
}

class SeqDom[D <: Dom](val elementDom: D, val length: Int) extends Dom {

  type Value = IndexedSeq[elementDom.Value]

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
  type Variable = SeqVar[D]
  def variable(name:String, offsets: Offsets = Offsets(), owner: Var[Dom]) = SeqVar(name, this,offsets,owner)
}

case class SeqVar[D <: Dom](name:String, domain: SeqDom[D], offsets: Offsets = Offsets(), owner: Var[Dom]) extends Var[SeqDom[D]] {

  val ranges = Ranges(offsets, offsets +(domain.elementDom.lengths, domain.length))
  val elements = for (i <- 0 until domain.length) yield
    domain.elementDom.variable(s"$name($i)", offsets + (domain.elementDom.lengths,i),owner)

}

case class Tuple2Var[D1 <: Dom, D2 <: Dom](name:String, domain:Tuple2Dom[D1,D2],offsets:Offsets, owner:Var[Dom]) extends Var[Tuple2Dom[D1,D2]] {
  val ranges = Ranges(offsets,offsets + domain.dom1.lengths + domain.dom2.lengths)
  val _1 = domain.dom1.variable(name + "._1",offsets,owner)
  val arg2 = domain.dom2.variable(name + "._2",offsets + domain.dom1.lengths,owner)
}

class Tuple2Dom[D1 <: Dom, D2 <: Dom](val dom1: D1, val dom2: D2) extends Dom {
  type Value = (dom1.Value, dom2.Value)
  type Variable = Tuple2Var[D1,D2]
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
  def variable(name:String, offsets: Offsets, owner: Var[Dom]) = Tuple2Var(name, this,offsets,owner)
}




trait Evaluator {
  def eval(inputs: Array[Setting], output: Setting)
}

trait Argmaxer {
  def argmax(observed: Setting, result: Setting)
}

trait Differentiator {
  def gradient(observed: Setting, current: Setting, outputError: Setting, gradient: Setting, value: Setting)
}


class VariableMapping(val srcIndex: Array[Int], val tgtIndex: Array[Int]) {
  def copyForward(src: Array[Setting], tgt: Array[Setting]) = {
    for (i <- 0 until srcIndex.length) tgt(tgtIndex(i)) := src(srcIndex(i))
  }
}

object VariableMapping {
  def apply(src: Array[Var[Dom]], tgt: Array[Var[Dom]]) = {
    val pairs = src.indices.view.map(i => i -> tgt.indexOf(src(i))).filter(_._2 != -1).toArray
    val (srcIndex, tgtIndex) = (pairs.map(_._1), pairs.map(_._2))
    new VariableMapping(srcIndex, tgtIndex)
  }
}

class Constant[D <: Dom](val domain: D, val value: D#Value) extends Term[D] {
  val vars      = Seq.empty
  val evaluator = new Evaluator {
    val result = domain.createSetting()
    domain.copyValue(value.asInstanceOf[domain.Value], result)
    def eval(inputs: Array[Setting], output: Setting) = {
      output := result
    }
  }
  def differentiator(variable: Var[Dom]) = ???
}


trait NAryOperator[D <: Dom] extends Term[D] {
  val vars = arguments.flatMap(_.vars).toSeq.distinct
  def arguments: Seq[Term[Dom]]
  def composer(): Evaluator
  def evaluator() = new Evaluator {
    val argOutputs = arguments.map(_.domain.createSetting()).toArray
    val argInputs  = arguments.map(_.vars.map(_.domain.createSetting()).toArray)
    val this2Arg   = arguments.map(a => VariableMapping(vars.toArray, a.vars.toArray))
    val argEvals   = arguments.map(_.evaluator())
    val comp       = composer()
    def eval(inputs: Array[Setting], output: Setting) = {
      for (i <- 0 until arguments.length) {
        this2Arg(i).copyForward(inputs, argInputs(i))
        argEvals(i).eval(argInputs(i), argOutputs(i))
      }
      comp.eval(argOutputs, output)
    }
  }
}


class DotProduct[T1 <: Term[VectorDom], T2 <: Term[VectorDom]](val arg1: T1, val arg2: T2) extends NAryOperator[DoubleDom] {

  val domain    = Dom.doubles
  val arguments = Seq(arg1, arg2)
  def composer() = new Evaluator {
    def eval(inputs: Array[Setting], output: Setting) = {
      output.cont(0) = inputs(0).vect(0) dot inputs(1).vect(1)
    }
  }
  def differentiator(variable: Var[Dom]) = new Differentiator {
    val d1 = arg1.differentiator(variable)
    val d2 = arg2.differentiator(variable)
    def gradient(fixed: Setting, current: Setting, outputError: Setting, gradient: Setting, value: Setting) = {
      //evaluate
    }
  }
}


object Playground {

  def main(args: Array[String]) {
    val pair = new Tuple2Dom(Dom.vectors, Dom.vectors).variable("pair")
    val x = Dom.vectors.variable("x")
    val c = new Constant[VectorDom](Dom.vectors, new DenseTensor1(Array(1.0)))
    val dot = new DotProduct(x, pair._1)
    val diff = dot.differentiator(x)

  }
}
