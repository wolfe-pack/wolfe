package ml.wolfe.term

import java.lang.System._

import cc.factorie.la.DenseTensor1
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
    require(args.length == vars.length, s"You need as many arguments as there are free variables (${vars.length})")
    val indexOfWrt = vars.indexOf(wrt)
    gradient(args, wrt = Seq(wrt))(indexOfWrt).asInstanceOf[wrt.domain.Value]
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

trait TermProxy[D <: Dom] extends Term[D] {
  def self: Term[D]
  val domain = self.domain
  def vars = self.vars
  def evaluator() = self.evaluator()
  def differentiator(wrt: Seq[Var[Dom]]) = self.differentiator(wrt)
  def atoms = self.atoms
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

sealed trait Atom
case class Atoms(disc: Seq[DiscVar[Any]] = Nil, cont: Seq[DoubleVar] = Nil, vect: Seq[VectorVar] = Nil, mats: Seq[MatrixVar] = Nil) {
  def ++(that: Atoms) = copy(disc = disc ++ that.disc, cont = cont ++ that.cont, vect = vect ++ that.vect, mats = mats ++ that.mats)

  def merge(that: Atoms) = copy(
    disc = (disc ++ that.disc).distinct,
    cont = (cont ++ that.cont).distinct,
    vect = (vect ++ that.vect).distinct,
    mats = (mats ++ that.mats).distinct)

  def filterByOwner(predicate: Var[Dom] => Boolean) = copy(
    disc = disc.filter(v => predicate(v.owner)),
    cont = cont.filter(v => predicate(v.owner)),
    vect = vect.filter(v => predicate(v.owner)),
    mats = mats.filter(v => predicate(v.owner))
  )

}

case class VectorVar(name: String, owner: Var[Dom], domain: VectorDom, offset: Int) extends Var[VectorDom] with Atom {
  val ranges = Ranges(Offsets(0, 0, offset, 0), Offsets(0, 0, offset + 1, 0))

  def atoms = Atoms(vect = List(this))
}

case class MatrixVar(name: String, owner: Var[Dom], domain: MatrixDom, offset: Int) extends Var[MatrixDom] with Atom {
  val ranges = Ranges(Offsets(0, 0, 0, offset), Offsets(0, 0, 0, offset + 1))

  def atoms = Atoms(mats = List(this))
}

case class DoubleVar(name: String, owner: Var[Dom], domain: DoubleDom, offset: Int) extends Var[DoubleDom] with Atom {
  self =>
  val ranges = Ranges(Offsets(0, offset, 0, 0), Offsets(0, offset + 1, 0, 0))

  def atoms = Atoms(cont = List(this))

  override def argmaxer(wrt: Seq[Var[Dom]]) = new Argmaxer {
    val contained = wrt.contains(self)

    def argmax(observed: Array[Setting], msgs: Array[Msgs], result: Array[Setting]) = {
      result(0).cont(0) = if (contained) Double.PositiveInfinity else observed(0).cont(0)
    }
  }
}

case class DiscVar[T](name: String, owner: Var[Dom], domain: DiscreteDom[T], offset: Int) extends Var[DiscreteDom[T]] with Atom {
  val ranges = Ranges(Offsets(offset, 0, 0, 0), Offsets(offset + 1, 0, 0, 0))

  def atoms = Atoms(disc = List(this.asInstanceOf[DiscVar[Any]]))


}


case class SeqVar[D <: Dom](name: String, domain: SeqDom[D],
                            offsets: Offsets = Offsets(),
                            owner: Var[Dom]) extends Var[SeqDom[D]] with SeqTerm[D] {


  def atoms = elements.view.map(_.atoms).foldLeft(Atoms())(_ ++ _)

  val ranges   = Ranges(offsets, offsets +(domain.elementDom.lengths, domain.length))
  val elements = for (i <- 0 until domain.length) yield
    domain.elementDom.variable(s"$name($i)", offsets +(domain.elementDom.lengths, i), if (owner == null) this else owner)

  /*
  def apply(generator:Generator[Int]) = new StochasticElement(generator) {
    val current = elements(generator.generate)

  }
  */


}

trait SeqTerm[D <: Dom] extends Term[SeqDom[D]] {
  def elements: IndexedSeq[domain.elementDom.Term]

  def apply(index: Int) = elements(index)

  def indices = elements.indices
  def length = elements.length

}

abstract class SeqTermImpl[D <: Dom] extends Composed[SeqDom[D]] with SeqTerm[D] {
  def arguments = elements

  def composer() = new Evaluator {

    def eval(inputs: Array[Setting], output: Setting) = {
      for (i <- 0 until inputs.length) {
        inputs(i).copyTo(output, domain.elementDom.lengths, i)
      }
    }
  }

  def differentiator(wrt: Seq[Var[Dom]]) = new ComposedDifferentiator {
    def localBackProp(argOutputs: Array[Setting], outError: Setting, gradient: Array[Setting]) = {
      //each argument will get its error signal from a subsection of the outError
      for (i <- 0 until argOutputs.length) {
        val offsets = domain.elementDom.lengths * i
        for (j <- 0 until domain.elementDom.lengths.contOff) {
          gradient(i).cont(j) = outError.cont(offsets.contOff + j)
        }
        for (j <- 0 until domain.elementDom.lengths.vectOff) {
          gradient(i).vect(j) := outError.vect(offsets.vectOff + j)
        }
      }
    }

    def withRespectTo = wrt
  }

  //val elements = arguments
}

trait Tuple2Term[D1 <: Dom, D2 <: Dom] extends Term[Tuple2Dom[D1, D2]] {
  def _1: domain.dom1.Term
  def _2: domain.dom2.Term
}

trait Tuple2TermImpl[D1 <: Dom, D2 <: Dom] extends Tuple2Term[D1, D2] with Composed[Tuple2Dom[D1, D2]] {
  def arguments = IndexedSeq(_1, _2)

  def composer() = ???

  def differentiator(wrt: Seq[Var[Dom]]) = ???
}

case class Tuple2Var[D1 <: Dom, D2 <: Dom](name: String,
                                           domain: Tuple2Dom[D1, D2],
                                           offsets: Offsets,
                                           owner: Var[Dom]) extends Var[Tuple2Dom[D1, D2]] with Tuple2Term[D1, D2] {

  val ranges               = Ranges(offsets, offsets + domain.dom1.lengths + domain.dom2.lengths)
  val _1: domain.dom1.Term = domain.dom1.variable(name + "._1", offsets, if (owner == null) this else owner)
  val _2: domain.dom2.Term = domain.dom2.variable(name + "._2", offsets + domain.dom1.lengths, if (owner == null) this else owner)

  def atoms = _1.atoms ++ _2.atoms
}


trait Evaluator {
  def eval(inputs: Array[Setting], output: Setting)
}

trait Differentiator {
  def term: Term[Dom]

  def withRespectTo: Seq[Var[Dom]]

  lazy val conditionedOn = term.vars.filterNot(withRespectTo.contains)

  lazy val activation = term.domain.createSetting()

  def forwardProp(current: Array[Setting])

  def backProp(error: Setting, gradient: Array[Setting])

  def gradient(singleVar:Var[Dom],args:Any*):singleVar.domain.Value = {
    val obj = term
    val errorSetting = obj.domain.toSetting(term.domain.one.asInstanceOf[obj.domain.Value])
    val argSettings = for ((a, v) <- args zip obj.vars) yield v.domain.toSetting(a.asInstanceOf[v.domain.Value])
    val result = obj.vars.map(_.domain.createZeroSetting()).toArray
    val output = obj.domain.createZeroSetting()
    addGradientAndValue(argSettings.toArray, errorSetting, result, output)
    val indexOfVar = obj.vars.indexOf(singleVar)
    obj.vars(indexOfVar).domain.toValue(result(indexOfVar)).asInstanceOf[singleVar.domain.Value]
  }

  def addGradientAndValue(current: Array[Setting], outputError: Setting, gradientAccumulator: Array[Setting], value: Setting): Unit = {
    //call forward propagate on term with current assignment to free variables
    forwardProp(current)
    //then backward propagate
    backProp(outputError, gradientAccumulator)
    //also return the value
    value := activation
  }
}

class EmptyDifferentiator(val term: Term[Dom], val withRespectTo: Seq[Var[Dom]]) extends Differentiator {
  val eval = term.evaluator()

  def forwardProp(current: Array[Setting]) = {
    eval.eval(current, activation)
  }

  def backProp(error: Setting, gradient: Array[Setting]) = {}
}

final class VariableMapping(val srcIndex: Array[Int], val tgtIndex: Array[Int]) {
  def copyForward(src: Array[Setting], tgt: Array[Setting]) = {
    for (i <- 0 until srcIndex.length) tgt(tgtIndex(i)) := src(srcIndex(i))
  }

  def copyBackward(src: Array[Setting], tgt: Array[Setting]) = {
    for (i <- 0 until srcIndex.length) src(srcIndex(i)) := tgt(tgtIndex(i))
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
        full2Arg(i).copyForward(inputs, argInputs(i))
        argEvals(i).eval(argInputs(i), argOutputs(i))
      }
      comp.eval(argOutputs, output)
    }
  }

  def atoms = arguments.map(_.atoms).foldLeft(Atoms())(_ ++ _)

  trait Composer {
    val argOutputs = arguments.map(_.domain.createSetting()).toArray
    val argInputs  = arguments.map(_.vars.map(_.domain.createSetting()).toArray)
    val full2Arg   = arguments.map(a => VariableMapping(vars, a.vars)).toArray
    val argEvals   = arguments.map(_.evaluator()).toArray
  }

  trait ComposedDifferentiator extends Differentiator with Composer {

    val term           = self
    val argErrors      = arguments.map(_.domain.createZeroSetting()).toArray
    val argGradients   = arguments.map(_.vars.map(_.domain.createZeroSetting()).toArray).toArray
    val argDiffs       = arguments.map(createDifferentiator).toArray
    val argActivations = argDiffs.map(_.activation)
    val comp           = composer()

    def createDifferentiator(term: Term[Dom]) =
      if (term.vars.exists(withRespectTo.contains)) term.differentiator(withRespectTo)
      else
        new EmptyDifferentiator(term, withRespectTo)


    def localBackProp(argOutputs: Array[Setting], outError: Setting, gradient: Array[Setting]): Unit

    //updates the activation of this term and all sub terms
    def forwardProp(current: Array[Setting]) = {
      for (i <- 0 until arguments.length) {
        full2Arg(i).copyForward(current, argInputs(i))
        argDiffs(i).forwardProp(argInputs(i))
      }
      comp.eval(argActivations, activation)

    }

    def backProp(error: Setting, gradient: Array[Setting]) = {
      localBackProp(argActivations, error, argErrors)
      for (i <- 0 until arguments.size) {
        if (arguments(i).vars.exists(withRespectTo.contains)) {
          full2Arg(i).copyForward(gradient, argGradients(i))
          argDiffs(i).backProp(argErrors(i), argGradients(i))
          full2Arg(i).copyBackward(gradient, argGradients(i))
        }
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
      gradient(0).cont(0) = result
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
      gradient(0).cont(0) = outError.cont(0) * 1.0 / argOutputs(0).cont(0)
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
      gradient(0).vect(0) := 0.0
      gradient(1).vect(0) := 0.0
      gradient(0).vect(0) +=(argOutputs(1).vect(0), scale)
      gradient(1).vect(0) +=(argOutputs(0).vect(0), scale)
    }
  }

}

class MatrixVectorProduct[T1 <: Term[MatrixDom], T2 <: Term[VectorDom]](val arg1: T1, val arg2: T2) extends Composed[VectorDom] {

  self =>

  val arguments = IndexedSeq(arg1, arg2)

  def composer() = new Evaluator {
    def eval(inputs: Array[Setting], output: Setting) = {
      output.vect(0) = inputs(0).mats(0) * inputs(1).vect(0)
    }
  }

  def differentiator(wrt: Seq[Var[Dom]]) = new ComposedDifferentiator {

    def withRespectTo = wrt

    def localBackProp(argOutputs: Array[Setting], outError: Setting, gradient: Array[Setting]): Unit = {
      val scale = outError.vect(0)
      gradient(0).mats(0) := 0.0
      gradient(1).vect(0) := 0.0
      gradient(0).mats(0) += argOutputs(1).vect(0) outer scale
      gradient(1).vect(0) += argOutputs(0).mats(0) * scale
    }
  }

  override val domain = new VectorDom(arg1.domain.dim1)
}


trait ComposedDoubleTerm extends DoubleTerm with Composed[DoubleDom] {
  val domain = Dom.doubles
}


trait UnaryTerm[T <: Term[Dom], D <: Dom] extends Composed[D] {
  def arg: T

  val arguments = IndexedSeq(arg)
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
      var zeros = 0
      var zeroIndex = -1
      for (i <- 0 until argOutputs.length) {
        //set to 0.0 by default
        gradient(i) := 0.0
        if (argOutputs(i).cont(0) == 0.0) {
          zeros += 1
          zeroIndex = i
        } else
          total *= argOutputs(i).cont(0)
      }
      if (zeros == 1) {
        gradient(zeroIndex).cont(0) = total
      } else if (zeros == 0) {
        for (i <- 0 until argOutputs.length)
          gradient(i).cont(0) = total / argOutputs(i).cont(0)
      }
    }
    def withRespectTo = wrt
  }
}


object Playground {

  def vector(values: Seq[Double]) = new DenseTensor1(values.toArray)

  import ml.wolfe.term.TermImplicits._

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

case class Offsets(discOff: Int = 0, contOff: Int = 0, vectOff: Int = 0, matsOff: Int = 0) {
  def +(disc: Int, cont: Int, vect: Int, mats: Int) = Offsets(discOff + disc, contOff + cont, vectOff + vect, matsOff + mats)

  def +(that: Offsets, scale: Int = 1) =
    Offsets(discOff + scale * that.discOff, contOff + scale * that.contOff, vectOff + scale * that.vectOff, matsOff + scale * that.matsOff)

  def *(scale: Int) = Offsets(scale * discOff, scale * contOff, scale * vectOff, scale * matsOff)
}

case class Ranges(from: Offsets, to: Offsets) {
  def copy(src: Setting, tgt: Setting): Unit = {
    arraycopy(src.disc, from.discOff, tgt.disc, 0, to.discOff - from.discOff)
    arraycopy(src.cont, from.contOff, tgt.cont, 0, to.contOff - from.contOff)
    arraycopy(src.vect, from.vectOff, tgt.vect, 0, to.vectOff - from.vectOff)
    arraycopy(src.mats, from.matsOff, tgt.mats, 0, to.matsOff - from.matsOff)
  }

  def addInto(src: Setting, tgt: Setting): Unit = {
    for (i <- 0 until numCont) {
      tgt.cont(from.contOff + i) += src.cont(i)
    }

    for (i <- 0 until numVect) {
      tgt.vect(from.vectOff + i) += src.vect(i)
    }

    for (i <- 0 until numMats) {
      tgt.mats(from.matsOff + i) += src.mats(i)
    }
  }

  def numDisc = to.discOff - from.discOff

  def numCont = to.contOff - from.contOff

  def numVect = to.vectOff - from.vectOff

  def numMats = to.matsOff - from.matsOff
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

class DiscreteEquals[T](var arg1: DiscreteTerm[T], var arg2: DiscreteTerm[T]) extends BinaryDiscreteOperator[BoolDom, DiscreteDom[T]] {
  val domain = Dom.bools

  def op(a1: Int, a2: Int) = if (a1 == a2) 1 else 0
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


