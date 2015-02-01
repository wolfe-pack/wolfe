package ml.wolfe.term

import java.lang.System._

import cc.factorie.la.DenseTensor1
import ml.wolfe.FactorieVector
import ml.wolfe.fg20._
import ml.wolfe.util.Math._


trait Term[+D <: Dom] {
  val domain: D
  def vars: Seq[Var[Dom]]
  def evaluator(): Evaluator
  def differentiator(wrt: Seq[Var[Dom]]): Differentiator
  def atoms: Atoms


  def argmaxer(wrt: Seq[Var[Dom]]): Argmaxer = {
    //todo: this could be type safe, for example by adding the argmax method to the RichDoubleTerm
    if (!domain.isDouble) sys.error("Argmax only supported for real valued terms")
    else if (wrt.forall(_.domain.isDiscrete)) new ExhaustiveSearchArgmaxer(this.asInstanceOf[DoubleTerm], wrt) else ???

  }


  def argmax[V <: Dom](wrt: Var[V], args: Any*): wrt.domain.Value = {
    val am = argmaxer(Seq(wrt))
    val observed = vars.filter(_ != wrt)
    val observedSettings = for ((a, v) <- args zip observed) yield v.domain.toSetting(a.asInstanceOf[v.domain.Value])
    val result = wrt.domain.createSetting()
    am.argmax(observedSettings.toArray, observed.map(_.domain.createMsgs()).toArray, Array(result))
    wrt.domain.toValue(result)
  }

  def apply(args: Any*): domain.Value = {
    val ev = evaluator()
    val output = domain.createSetting()
    val argSettings = for ((a, v) <- args zip vars) yield v.domain.toSetting(a.asInstanceOf[v.domain.Value])
    ev.eval(argSettings.toArray, output)
    domain.toValue(output)
  }

  def gradient[V <: Dom](wrt: Var[V], args: Any*): wrt.domain.Value = {
    gradient(args, wrt = Seq(wrt))(0).asInstanceOf[wrt.domain.Value]
  }

  def gradient(args: Seq[Any], error: domain.Value = domain.one, wrt: Seq[Var[Dom]] = vars): Seq[Any] = {
    val diff = differentiator(wrt)
    val errorSetting = domain.toSetting(error)
    val argSettings = for ((a, v) <- args zip vars) yield v.domain.toSetting(a.asInstanceOf[v.domain.Value])
    val result = vars.map(_.domain.createZeroSetting()).toArray
    val output = domain.createZeroSetting()
    diff.addGradientAndValue(argSettings.toArray, errorSetting, result, output)
    for ((s, v) <- result zip vars) yield v.domain.toValue(s)
  }
}


trait Var[+D <: Dom] extends Term[D] {
  self =>
  def name: String
  def ranges: Ranges
  def owner: Var[Dom]
  def vars = if (owner == null) Seq(this) else Seq(owner)
  def differentiator(wrt: Seq[Var[Dom]]) = new Differentiator {
    def term = self
    def withRespectTo = wrt
    def forwardProp(current: Array[Setting]) = {
      ranges.copy(current(0), activation)
    }
    def backProp(error: Setting, gradient: Array[Setting]) = {
      ranges.addInto(error, gradient(0))
    }
  }
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

  val lengths = Offsets(0, 0, 1)

  def toValue(setting: Setting, offsets: Offsets) =
    setting.vect(offsets.vectOff)
  def copyValue(value: Value, setting: Setting, offsets: Offsets) =
    setting.vect(offsets.vectOff) = value
  def variable(name: String, offsets: Offsets, owner: Var[Dom]) = VectorVar(name, owner, this, offsets.vectOff)
  def one = new DenseTensor1(dim, 1.0)
  def zero = new DenseTensor1(dim, 0.0)
}

case class Atoms(disc: Seq[DiscVar[Any]] = Nil, cont: Seq[DoubleVar] = Nil, vect: Seq[VectorVar] = Nil) {
  def ++(that: Atoms) = copy(disc = disc ++ that.disc, cont = cont ++ that.cont, vect = vect ++ that.vect)
  def merge(that: Atoms) = copy(
    disc = (disc ++ that.disc).distinct,
    cont = (cont ++ that.cont).distinct,
    vect = (vect ++ that.vect).distinct)
}

case class VectorVar(name: String, owner: Var[Dom], domain: VectorDom, offset: Int) extends Var[VectorDom] {
  val ranges = Ranges(Offsets(0, 0, offset), Offsets(0, 0, offset + 1))
  def atoms = Atoms(vect = List(this))
}
case class DoubleVar(name: String, owner: Var[Dom], domain: DoubleDom, offset: Int) extends Var[DoubleDom] {
  self =>
  val ranges = Ranges(Offsets(0, offset, 0), Offsets(0, offset + 1, 0))
  def atoms = Atoms(cont = List(this))
  override def argmaxer(wrt: Seq[Var[Dom]]) = new Argmaxer {
    val contained = wrt.contains(self)
    def argmax(observed: Array[Setting], msgs: Array[Msgs], result: Array[Setting]) = {
      result(0).cont(0) = if (contained) Double.PositiveInfinity else observed(0).cont(0)
    }
  }
}

case class DiscVar[T](name: String, owner: Var[Dom], domain: DiscreteDom[T], offset: Int) extends Var[DiscreteDom[T]] {
  val ranges = Ranges(Offsets(offset, 0, 0), Offsets(offset + 1, 0, 0))
  def atoms = Atoms(disc = List(this.asInstanceOf[DiscVar[Any]]))


}

class DoubleDom extends Dom {
  type Value = Double
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
  def toValue(setting: Setting, offsets: Offsets = Offsets()) =
    values(setting.disc(offsets.discOff))
  def copyValue(value: Value, setting: Setting, offsets: Offsets = Offsets()) =
    setting.disc(offsets.discOff) = values.indexOf(value)
  val lengths = Offsets(1, 0, 0)
  type Variable = DiscVar[T]
  def variable(name: String, offsets: Offsets = Offsets(), owner: Var[Dom]) = DiscVar(name, owner, this, offsets.discOff)
  def one = values.last
  def zero = values.head
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
  def variable(name: String, offsets: Offsets = Offsets(), owner: Var[Dom]) = SeqVar(name, this, offsets, owner)
  def one = for (i <- 0 until length) yield elementDom.one
  def zero = for (i <- 0 until length) yield elementDom.zero

}

case class SeqVar[D <: Dom](name: String, domain: SeqDom[D], offsets: Offsets = Offsets(), owner: Var[Dom]) extends Var[SeqDom[D]] {


  def atoms = elements.view.map(_.atoms).foldLeft(Atoms())(_ ++ _)
  val ranges   = Ranges(offsets, offsets +(domain.elementDom.lengths, domain.length))
  val elements = for (i <- 0 until domain.length) yield
    domain.elementDom.variable(s"$name($i)", offsets +(domain.elementDom.lengths, i), if (owner == null) this else owner)

}

case class Tuple2Var[D1 <: Dom, D2 <: Dom](name: String, domain: Tuple2Dom[D1, D2], offsets: Offsets, owner: Var[Dom]) extends Var[Tuple2Dom[D1, D2]] {
  val ranges = Ranges(offsets, offsets + domain.dom1.lengths + domain.dom2.lengths)
  val _1     = domain.dom1.variable(name + "._1", offsets, if (owner == null) this else owner)
  val _2     = domain.dom2.variable(name + "._2", offsets + domain.dom1.lengths, if (owner == null) this else owner)
  def atoms = _1.atoms ++ _2.atoms
}

class Tuple2Dom[D1 <: Dom, D2 <: Dom](val dom1: D1, val dom2: D2) extends Dom {
  type Value = (dom1.Value, dom2.Value)
  type Variable = Tuple2Var[D1, D2]
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


trait Evaluator {
  def eval(inputs: Array[Setting], output: Setting)
}

trait Argmaxer {
  def argmax(observed: Array[Setting], msgs: Array[Msgs], result: Array[Setting])
}

trait Differentiator {
  def term: Term[Dom]
  def withRespectTo: Seq[Var[Dom]]
  lazy val conditionedOn = term.vars.filterNot(withRespectTo.contains)

  lazy val activation = term.domain.createSetting()
  lazy val eval       = term.evaluator()

  def forwardProp(current: Array[Setting])
  def backProp(error: Setting, gradient: Array[Setting])

  def addGradientAndValue(current: Array[Setting], outputError: Setting, gradient: Array[Setting], value: Setting): Unit = {
    //call forward propagate on term with current assignment to free variables
    forwardProp(current)
    //then backward propagate
    backProp(outputError, gradient)
    //also return the value
    value := activation
  }

}


class VariableMapping(val srcIndex: Array[Int], val tgtIndex: Array[Int]) {
  def copyForward(src: Array[Setting], tgt: Array[Setting]) = {
    for (i <- 0 until srcIndex.length) tgt(tgtIndex(i)) := src(srcIndex(i))
  }
}

object VariableMapping {
  def apply(src: Seq[Var[Dom]], tgt: Seq[Var[Dom]]) = {
    val pairs = src.indices.view.map(i => i -> tgt.indexOf(src(i))).filter(_._2 != -1).toArray
    val (srcIndex, tgtIndex) = (pairs.map(_._1), pairs.map(_._2))
    new VariableMapping(srcIndex, tgtIndex)
  }
}

class Constant[D <: Dom](val domain: D, val value: D#Value) extends Term[D] {
  self =>
  val vars      = Seq.empty
  val evaluator = new Evaluator {
    val result = domain.toSetting(value.asInstanceOf[domain.Value])
    def eval(inputs: Array[Setting], output: Setting) = {
      output := result
    }
  }
  def atoms = Atoms()
  def differentiator(wrt: Seq[Var[Dom]]) = new Differentiator {
    val result = domain.toSetting(value.asInstanceOf[domain.Value])
    def forwardProp(current: Array[Setting]) = activation := result
    def term = self
    def withRespectTo = wrt
    def backProp(error: Setting, gradient: Array[Setting]) = {}
  }
}


trait Composed[D <: Dom] extends Term[D] {

  self =>

  lazy val vars = arguments.flatMap(_.vars).toSeq.distinct
  def arguments: IndexedSeq[Term[Dom]]
  def composer(): Evaluator
  def evaluator() = new Evaluator with Composer {
    val comp = composer()
    def eval(inputs: Array[Setting], output: Setting) = {
      for (i <- 0 until arguments.length) {
        this2Arg(i).copyForward(inputs, argInputs(i))
        argEvals(i).eval(argInputs(i), argOutputs(i))
      }
      comp.eval(argOutputs, output)
    }
  }
  def atoms = arguments.map(_.atoms).foldLeft(Atoms())(_ ++ _)

  trait Composer {
    val argOutputs = arguments.map(_.domain.createSetting()).toArray
    val argInputs  = arguments.map(_.vars.map(_.domain.createSetting()).toArray)
    val this2Arg   = arguments.map(a => VariableMapping(vars, a.vars)).toArray
    val argEvals   = arguments.map(_.evaluator()).toArray
  }

  trait ComposedDifferentiator extends Differentiator with Composer {
    def localBackProp(argOutputs: Array[Setting], outError: Setting, gradient: Array[Setting]): Unit

    val term           = self
    val argErrors      = arguments.map(_.domain.createZeroSetting()).toArray
    val argGradients   = arguments.map(_.vars.map(_.domain.createZeroSetting()).toArray).toArray
    val argDiffs       = arguments.map(_.differentiator(withRespectTo)).toArray
    val argActivations = argDiffs.map(_.activation)
    val comp           = composer()


    //updates the activation of this term and all sub terms
    def forwardProp(current: Array[Setting]) = {
      for (i <- 0 until arguments.length) {
        this2Arg(i).copyForward(current, argInputs(i))
        argDiffs(i).forwardProp(argInputs(i))
      }
      comp.eval(argActivations, activation)

    }

    def backProp(error: Setting, gradient: Array[Setting]) = {
      localBackProp(argActivations, error, argErrors)
      for (i <- 0 until arguments.size) {
        if (arguments(i).vars.exists(withRespectTo.contains))
          argDiffs(i).backProp(argErrors(i), gradient)
      }
    }
  }

}


class Sigmoid[T <: Term[DoubleDom]](val arg: T) extends ComposedDoubleTerm {
  self =>

  val arguments = IndexedSeq(arg)
  def composer() = new Evaluator {
    def eval(inputs: Array[Setting], output: Setting) = {
      output.cont(0) = sigmoid(inputs(0).cont(0))
    }
  }
  def differentiator(wrt: Seq[Var[Dom]]) = new ComposedDifferentiator {
    def localBackProp(argOutputs: Array[Setting], outError: Setting, gradient: Array[Setting]) = {
      val sigm = sigmoid(argOutputs(0).cont(0))
      val result = (1.0 - sigm) * sigm * outError.cont(0)
      gradient(0).cont(0) += result
    }
    def withRespectTo = wrt
  }
}

class Log[T <: DoubleTerm](val arg: T) extends ComposedDoubleTerm {
  val arguments = IndexedSeq(arg)
  def composer() = new Evaluator {
    def eval(inputs: Array[Setting], output: Setting) = output.cont(0) = math.log(inputs(0).cont(0))
  }
  def differentiator(wrt: Seq[Var[Dom]]) = new ComposedDifferentiator {
    def localBackProp(argOutputs: Array[Setting], outError: Setting, gradient: Array[Setting]) = {
      gradient(0).cont(0) += outError.cont(0) * 1.0 / argOutputs(0).cont(0)
    }
    def withRespectTo = wrt
  }
}

class DotProduct[T1 <: Term[VectorDom], T2 <: Term[VectorDom]](val arg1: T1, val arg2: T2) extends ComposedDoubleTerm {

  self =>

  val arguments = IndexedSeq(arg1, arg2)
  def composer() = new Evaluator {
    def eval(inputs: Array[Setting], output: Setting) = {
      output.cont(0) = inputs(0).vect(0) dot inputs(1).vect(0)
    }
  }


  def differentiator(wrt: Seq[Var[Dom]]) = new ComposedDifferentiator {

    def withRespectTo = wrt


    def localBackProp(argOutputs: Array[Setting], outError: Setting, gradient: Array[Setting]): Unit = {
      val scale = outError.cont(0)
      gradient(0).vect(0) +=(argOutputs(1).vect(0), scale)
      gradient(1).vect(0) +=(argOutputs(0).vect(0), scale)
    }
  }

}

object TermImplicits {

  val doubles = Dom.doubles
  val bools   = Dom.bools
  def vectors(dim: Int) = new VectorDom(dim)
  def discrete[T](args: T*) = new DiscreteDom[T](args.toIndexedSeq)
  def vector(values: Double*) = new DenseTensor1(values.toArray)

  def sigm[T <: DoubleTerm](term: T) = new Sigmoid(term)

  def log[T <: DoubleTerm](term: T) = new Log(term)

  def I[T <: BoolTerm](term: T) = new Iverson(term)

  implicit def doubleToConstant(d: Double): Constant[DoubleDom] = new Constant[DoubleDom](Dom.doubles, d)

  def argmax[D <: Dom](dom: D)(obj: dom.Variable => DoubleTerm): dom.Value = {
    val variable = dom.variable("_hidden")
    val term = obj(variable)
    term.argmax(variable).asInstanceOf[dom.Value]
  }

  implicit class RichDoubleTerm(term: DoubleTerm) {
    def +(that: DoubleTerm) = new Sum(IndexedSeq(term, that))
    def *(that: DoubleTerm): Product = new Product(IndexedSeq(term, that))
  }

  implicit class RichBoolTerm(term: BoolTerm) {
    def &&(that: BoolTerm) = new And(term, that)
    def ||(that: BoolTerm) = new Or(term, that)
    def ->(that: BoolTerm) = new Implies(term, that)
  }

  implicit class RichTerm[D <: Dom](val term: Term[D]) {
    def apply(args: Any*) = term.apply(args)
  }

  implicit class RichDom[D <: Dom](val dom: D) {
    def x[D2 <: Dom](that: D2) = new Tuple2Dom[D, D2](dom, that)
  }

  implicit class RichVectTerm(val vect: Term[VectorDom]) {
    def dot(that: Term[VectorDom]) = new DotProduct(vect, that)
  }

}

trait ComposedDoubleTerm extends DoubleTerm with Composed[DoubleDom] {
  val domain = Dom.doubles
}

trait UnaryTerm[T <: Term[Dom], D <: Dom] extends Composed[D] {
  def arg: T
  val arguments = IndexedSeq(arg)
}

class Sum(val arguments: IndexedSeq[DoubleTerm]) extends ComposedDoubleTerm {
  def composer() = new Evaluator {
    def eval(inputs: Array[Setting], output: Setting) = {
      output.cont(0) = 0.0
      for (i <- 0 until inputs.length)
        output.cont(0) += inputs(i).cont(0)
    }
  }
  def differentiator(wrt: Seq[Var[Dom]]) = new ComposedDifferentiator {
    def localBackProp(argOutputs: Array[Setting], outError: Setting, gradient: Array[Setting]) = {
      for (i <- 0 until argOutputs.length)
        gradient(i).cont(0) += outError.cont(0)
    }
    def withRespectTo = wrt
  }
}
class Product(val arguments: IndexedSeq[DoubleTerm]) extends ComposedDoubleTerm {
  def composer() = new Evaluator {
    def eval(inputs: Array[Setting], output: Setting) = {
      output.cont(0) = 1.0
      for (i <- 0 until inputs.length)
        output.cont(0) *= inputs(i).cont(0)
    }
  }
  def differentiator(wrt: Seq[Var[Dom]]) = new ComposedDifferentiator {
    def localBackProp(argOutputs: Array[Setting], outError: Setting, gradient: Array[Setting]) = {
      var total = outError.cont(0)
      for (i <- 0 until argOutputs.length)
        total *= argOutputs(i).cont(0)
      for (i <- 0 until argOutputs.length)
        gradient(i).cont(0) += total / argOutputs(i).cont(0)
    }
    def withRespectTo = wrt
  }
}


object Playground {

  def vector(values: Seq[Double]) = new DenseTensor1(values.toArray)

  import TermImplicits._

  def main(args: Array[String]) {

    val test = argmax(doubles) { x => x}
    println(test)

    //    val X = new VectorDom(1)
    //    val XX = new Tuple2Dom(X, X)
    //    val pair = XX.variable("pair")
    //    val x = X.variable("x")
    //    val dot = new DotProduct(x, pair._1)
    //    val diff = dot.differentiator(Seq(x))
    //
    //    println(dot.vars)
    //    println(dot(Seq(vector(Seq(2.0)), (vector(Seq(10.0)), vector(Seq(2.0))))))


    //val result = dot(Seq())

  }
}

case class Offsets(discOff: Int = 0, contOff: Int = 0, vectOff: Int = 0) {
  def +(disc: Int, cont: Int, vect: Int) = Offsets(discOff + disc, contOff + cont, vectOff + vect)
  def +(that: Offsets, scale: Int = 1) =
    Offsets(discOff + scale * that.discOff, contOff + scale * that.contOff, vectOff + scale * that.vectOff)
  def *(scale: Int) = Offsets(scale * discOff, scale * contOff, scale * vectOff)
}
case class Ranges(from: Offsets, to: Offsets) {
  def copy(src: Setting, tgt: Setting): Unit = {
    arraycopy(src.disc, from.discOff, tgt.disc, 0, to.discOff - from.discOff)
    arraycopy(src.cont, from.contOff, tgt.cont, 0, to.contOff - from.contOff)
    arraycopy(src.vect, from.vectOff, tgt.vect, 0, to.vectOff - from.vectOff)
  }

  def addInto(src: Setting, tgt: Setting): Unit = {
    for (i <- 0 until numCont) {
      tgt.cont(from.contOff + i) += src.cont(i)
    }
    for (i <- 0 until numVect) {
      tgt.vect(from.vectOff + i) += src.vect(i)
    }
  }

  def numDisc = to.discOff - from.discOff
  def numCont = to.contOff - from.contOff
  def numVect = to.vectOff - from.vectOff
}

class Iverson[T <: BoolTerm](val arg: T) extends UnaryTerm[T, DoubleDom] with ComposedDoubleTerm {
  def composer() = new Evaluator {
    def eval(inputs: Array[Setting], output: Setting) = {
      output.cont(0) = if (inputs(0).disc(0) == 0) 0.0 else 1.0
    }
  }
  def differentiator(wrt: Seq[Var[Dom]]) = ???
}

trait BinaryDiscreteOperator[D <: Dom, A <: Dom] extends Composed[D] {

  def arg1: Term[A]
  def arg2: Term[A]
  def op(a1: Int, a2: Int): Int

  val arguments = IndexedSeq(arg1, arg2)
  def composer() = new Evaluator {
    def eval(inputs: Array[Setting], output: Setting) = {
      output.disc(0) = op(inputs(0).disc(0), inputs(1).disc(0))
    }
  }
  def differentiator(wrt: Seq[Var[Dom]]) = ???
}

class And(val arg1: BoolTerm, val arg2: BoolTerm) extends BinaryDiscreteOperator[BoolDom, BoolDom] {
  def op(a1: Int, a2: Int) = if (a1 != 0 && a2 != 0) 1 else 0
  val domain = Dom.bools
}

class Or(val arg1: BoolTerm, val arg2: BoolTerm) extends BinaryDiscreteOperator[BoolDom, BoolDom] {
  def op(a1: Int, a2: Int) = if (a1 != 0 || a2 != 0) 1 else 0
  val domain = Dom.bools
}

class Implies(val arg1: BoolTerm, val arg2: BoolTerm) extends BinaryDiscreteOperator[BoolDom, BoolDom] {
  def op(a1: Int, a2: Int) = if (a1 == 0 || a2 != 0) 1 else 0
  val domain = Dom.bools
}


