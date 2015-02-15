package ml.wolfe.term

import cc.factorie.la.DenseTensor1
import ml.wolfe.util.Math._

import scala.collection.mutable

/**
 * A term is an object that represents a computation of a value based on an assignment to free variables in the term.
 * The term is typed, in the sense that it is guaranteed to produce values in a domain [[ml.wolfe.term.Dom]].
 * The term's computation is defined through the term's evaluator object. Terms can also provide means to differentiate,
 * optimize or marginalize the term with respect to some variables.
 * @tparam D the type of domain this term produces values in.
 */
trait Term[+D <: Dom] extends TermHelper[D] {

  /**
   * The domain of the term. Evaluating the term will yield a value within this domain.
   */
  val domain: D

  /**
   * The sequence of free variables in this term. Notice that the order of this sequence determines the order
   * of arguments to be passed into the evaluation methods.
   * @return the sequence of free variables in this term.
   */
  def vars: Seq[Var[Dom]]

  /**
   * The evaluator calculates the value of the term given an assignment to free variables.
   * @return the term's evaluator.
   */
  def evaluator(): Evaluator

  /**
   * Variables may describe structured objects. Terms may only depend on parts of these structured objects. Atoms
   * define a set of such parts.
   * @return the atoms that this term depend on.
   */
  def atoms: Atoms = Atoms.fromIterator(atomsIterator)

  def atomsIterator: Iterator[Atom[Dom]]

  def differentiator(wrt: Seq[Var[Dom]]): Differentiator

  def maxMarginalizer(wrt: Seq[Var[Dom]], target: Seq[Var[Dom]]): MaxMarginalizer = {
    //todo: this could be type safe, for example by adding the argmax method to the RichDoubleTerm
    if (!domain.isDouble) sys.error("Argmax only supported for real valued terms")
    else if (wrt.forall(_.domain.isDiscrete))
      new ExhaustiveSearchMaxMarginalizer(this.asInstanceOf[DoubleTerm], wrt, target)
    else ???
  }

  def argmaxer(wrt: Seq[Var[Dom]]): Argmaxer = {
    //todo: this could be type safe, for example by adding the argmax method to the RichDoubleTerm
    if (!domain.isDouble) sys.error("Argmax only supported for real valued terms")
    else if (wrt.forall(_.domain.isDiscrete)) new ExhaustiveSearchArgmaxer(this.asInstanceOf[DoubleTerm], wrt) else ???
  }

}

/**
 * Provides a range of convenience methods to access Term functionality in more type safe ways.
 * @tparam D
 */
trait TermHelper[+D <: Dom] {
  this: Term[D] =>

  def maxMarginals[T <: Dom](target: Var[T], wrt: Var[T])(incoming: wrt.domain.Marginals)(args: Any*): target.domain.Marginals = {
    val maxMarger = maxMarginalizer(Seq(wrt), Seq(target))
    val observed = vars.filter(v => v != wrt && v != target)
    val observedSettings = Dom.createSettings(observed, args)
    val result = target.domain.createZeroMsgs()
    val incomingMsgs = Array(wrt.domain.toMsgs(incoming))
    maxMarger.maxMarginals(observedSettings, incomingMsgs, Array(result))
    target.domain.toMarginals(result)
  }

  def argmax[V <: Dom](wrt: Var[V], args: Any*): wrt.domain.Value = {
    val am = argmaxer(Seq(wrt))
    val observed = vars.filter(_ != wrt)
    val observedSettings = for ((a, v) <- args zip observed) yield v.domain.toSetting(a.asInstanceOf[v.domain.Value])
    val result = wrt.domain.createSetting()
    am.argmax(observedSettings.toArray, observed.map(_.domain.createMsgs()).toArray, Array(result))
    wrt.domain.toValue(result)
  }


  def eval(args: Any*): domain.Value = {
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

trait ProxyTerm[D <: Dom] extends Term[D] {
  def self: Term[D]

  val domain = self.domain

  def vars = self.vars

  def evaluator() = self.evaluator()

  def differentiator(wrt: Seq[Var[Dom]]) = self.differentiator(wrt)

  def atomsIterator = self.atomsIterator
}


trait Var[+D <: Dom] extends Term[D] {
  self =>
  def name: String

  def ranges: Ranges

  def owner: Var[Dom]

  def ownerOrSelf: Var[Dom] = if (owner == null) self else owner

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

    def eval(inputs: Array[Setting], output: Setting) = {
      ranges.copy(inputs(0), output)
    }
  }
}

trait Atom[+D <: Dom] extends Var[D] {
  def offset: Int
  def atomsIterator = Iterator(this)

}

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

  def distinct = copy(
    disc = disc.distinct,
    cont = cont.distinct,
    vect = vect.distinct,
    mats = mats.distinct
  )

}

object Atoms {
  def fromIterator(iterator:Iterator[Atom[Dom]]) = {
    val cont = new mutable.HashSet[DoubleVar]()
    val vect = new mutable.HashSet[VectorVar]()
    val disc = new mutable.HashSet[DiscVar[Any]]()
    val mats = new mutable.HashSet[MatrixVar]()

    for (a <- iterator) a match {
      case c:DoubleVar => cont += c
      case v:VectorVar => vect += v
      case m:MatrixVar => mats += m
      case d:DiscVar[_] => disc += d.asInstanceOf[DiscVar[Any]]
    }
    Atoms(disc.toSeq,cont.toSeq,vect.toSeq,mats.toSeq)
  }
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

  def gradient(singleVar: Var[Dom], args: Any*): singleVar.domain.Value = {
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


class DoubleFun[T <: Term[DoubleDom]](val arg: T, fun: Double => Double, deriv: Double => Double) extends ComposedDoubleTerm {
  self =>

  type ArgumentType = T

  val arguments = IndexedSeq(arg)


  def copy(args: IndexedSeq[ArgumentType]) = new DoubleFun[T](args(0), fun, deriv)

  def composer() = new Evaluator {
    def eval(inputs: Array[Setting], output: Setting) = {
      output.cont(0) = fun(inputs(0).cont(0))
    }
  }

  def differentiator(wrt: Seq[Var[Dom]]) = new ComposedDifferentiator {
    def localBackProp(argOutputs: Array[Setting], outError: Setting, gradient: Array[Setting]) = {
      gradient(0).cont(0) = deriv(argOutputs(0).cont(0)) * outError.cont(0)
    }

    def withRespectTo = wrt
  }
}

class Sigmoid[T <: Term[DoubleDom]](override val arg: T) extends DoubleFun(arg, sigmoid, sigmoidDeriv)

class Log[T <: DoubleTerm](override val arg: T) extends DoubleFun(arg, math.log, logDeriv)

class Tanh[T <: DoubleTerm](override val arg: T) extends DoubleFun(arg, tanh, tanhDeriv)


class VectorDoubleFun[T <: Term[VectorDom]](val arg: T, fun: Double => Double, deriv: Double => Double) extends Composed[VectorDom] {
  self =>

  type ArgumentType = T

  val arguments = IndexedSeq(arg)

  val domain: arg.domain.type = arg.domain

  def copy(args: IndexedSeq[ArgumentType]) = new VectorDoubleFun[T](args(0), fun, deriv)

  def composer() = new Evaluator {
    def eval(inputs: Array[Setting], output: Setting) = {
      output.vect(0) = new DenseTensor1(inputs(0).vect(0) map fun)
    }
  }

  def differentiator(wrt: Seq[Var[Dom]]) = new ComposedDifferentiator {
    def localBackProp(argOutputs: Array[Setting], outError: Setting, gradient: Array[Setting]) = {
      gradient(0).vect(0) =
        new DenseTensor1(argOutputs(0).vect(0) map deriv) * outError.vect(0).asInstanceOf[DenseTensor1]
    }

    def withRespectTo = wrt
  }

}

class VectorSigmoid[T <: Term[VectorDom]](override val arg: T)
  extends VectorDoubleFun(arg, sigmoid, sigmoidDeriv)

class VectorTanh[T <: Term[VectorDom]](override val arg: T)
  extends VectorDoubleFun(arg, tanh, tanhDeriv)

class DotProduct[T1 <: Term[VectorDom], T2 <: Term[VectorDom]](val arg1: T1, val arg2: T2) extends ComposedDoubleTerm {

  self =>

  type ArgumentType = Term[VectorDom]

  val arguments = IndexedSeq(arg1, arg2)


  def copy(args: IndexedSeq[ArgumentType]) = new DotProduct(args(0), args(1))

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

//rockt: this should be generalized to vectors and matrices
class MatrixDotProduct[T1 <: Term[MatrixDom], T2 <: Term[MatrixDom]](val arg1: T1, val arg2: T2) extends ComposedDoubleTerm {
  self =>


  type ArgumentType = Term[MatrixDom]

  val arguments = IndexedSeq(arg1, arg2)


  def copy(args: IndexedSeq[ArgumentType]) = new MatrixDotProduct(args(0), args(1))

  def composer() = new Evaluator {
    def eval(inputs: Array[Setting], output: Setting) = {
      output.cont(0) = inputs(0).mats(0) dot inputs(1).mats(0)
    }
  }


  def differentiator(wrt: Seq[Var[Dom]]) = new ComposedDifferentiator {

    def withRespectTo = wrt


    def localBackProp(argOutputs: Array[Setting], outError: Setting, gradient: Array[Setting]): Unit = {
      val scale = outError.cont(0)
      gradient(0).mats(0) := 0.0
      gradient(1).mats(0) := 0.0
      gradient(0).mats(0) +=(argOutputs(1).mats(0), scale)
      gradient(1).mats(0) +=(argOutputs(0).mats(0), scale)
    }
  }
}

class VectorScaling[T1 <: VectorTerm, T2 <: DoubleTerm](val arg1: T1, arg2: T2) extends Composed[VectorDom] {

  self =>


  type ArgumentType = Term[Dom]

  val arguments = IndexedSeq(arg1, arg2)


  def copy(args: IndexedSeq[ArgumentType]) =
    new VectorScaling(args(0).asInstanceOf[VectorTerm], args(1).asInstanceOf[DoubleTerm])

  override val domain: VectorDom = new VectorDom(arg1.domain.dim)

  def composer() = new Evaluator {
    def eval(inputs: Array[Setting], output: Setting) = {
      output.vect(0) = inputs(0).vect(0) * inputs(1).cont(0)
    }
  }

  def differentiator(wrt: Seq[Var[Dom]]) = new ComposedDifferentiator {

    def withRespectTo = wrt

    def localBackProp(argOutputs: Array[Setting], outError: Setting, gradient: Array[Setting]): Unit = {
      val error = outError.vect(0)
      gradient(0).vect(0) := 0.0
      gradient(0).vect(0) += error * argOutputs(1).cont(0)
      gradient(1).cont(0) += argOutputs(0).vect(0) dot error
    }
  }
}

class VectorConcatenation[T1 <: Term[VectorDom], T2 <: Term[VectorDom]](val arg1: T1, val arg2: T2) extends Composed[VectorDom] {

  self =>


  type ArgumentType = Term[VectorDom]

  val arguments = IndexedSeq(arg1, arg2)


  def copy(args: IndexedSeq[ArgumentType]) = new VectorConcatenation(args(0), args(1))

  override val domain: VectorDom = new VectorDom(arg1.domain.dim + arg2.domain.dim)

  def composer() = new Evaluator {
    def eval(inputs: Array[Setting], output: Setting) = {
      //fixme: slow!
      output.vect(0) = new DenseTensor1((inputs(0).vect(0).asSeq ++ inputs(1).vect(0).asSeq).toArray)
    }
  }


  def differentiator(wrt: Seq[Var[Dom]]) = new ComposedDifferentiator {

    def withRespectTo = wrt

    def localBackProp(argOutputs: Array[Setting], outError: Setting, gradient: Array[Setting]): Unit = {
      val upstream = outError.vect(0)
      gradient(0).vect(0) := 0.0
      gradient(1).vect(0) := 0.0
      gradient(0).vect(0) += new DenseTensor1(upstream.asArray.slice(0, argOutputs(0).vect(0).dim1))
      gradient(1).vect(0) += new DenseTensor1(upstream.asArray.slice(argOutputs(0).vect(0).dim1, upstream.dim1))
    }
  }

}

class MatrixVectorProduct[T1 <: MatrixTerm, T2 <: VectorTerm](val arg1: T1, val arg2: T2) extends Composed[VectorDom] {

  self =>

  import ml.wolfe.util.PimpMyFactorie._


  type ArgumentType = Term[Dom]

  val arguments = IndexedSeq(arg1, arg2)

  override val domain = new VectorDom(arg1.domain.dim1)


  def copy(args: IndexedSeq[ArgumentType]) =
    new MatrixVectorProduct(args(0).asInstanceOf[MatrixTerm], args(1).asInstanceOf[VectorTerm])

  def composer() = new Evaluator {
    def eval(inputs: Array[Setting], output: Setting) = {
      output.vect(0) = inputs(0).mats(0) * inputs(1).vect(0)
    }
  }

  def differentiator(wrt: Seq[Var[Dom]]) = new ComposedDifferentiator {

    def withRespectTo = wrt

    def localBackProp(argOutputs: Array[Setting], outError: Setting, gradient: Array[Setting]): Unit = {
      val A = argOutputs(0).mats(0)
      val x = argOutputs(1).vect(0)
      val error = outError.vect(0)
      require(A.dim2 == x.dim1, s"dimensions don't match: ${A.toDimensionsString} * ${x.dim1}")
      require(A.dim1 == error.dim1, s"dimensions don't match: ${A.toDimensionsString} * ${x.dim1} => ${error.dim1}")

      gradient(0).mats(0) := 0.0
      gradient(1).vect(0) := 0.0
      gradient(0).mats(0) += error outer x
      gradient(1).vect(0) += A.t * error
    }
  }
}


trait ComposedDoubleTerm extends DoubleTerm with Composed[DoubleDom] {
  val domain = Dom.doubles
}


trait UnaryTerm[T <: Term[Dom], D <: Dom] extends Composed[D] {
  def arg: T

  type ArgumentType = T

  val arguments = IndexedSeq(arg)
}


class Product(val arguments: IndexedSeq[DoubleTerm]) extends ComposedDoubleTerm {

  type ArgumentType = DoubleTerm

  def composer() = new Evaluator {
    def eval(inputs: Array[Setting], output: Setting) = {
      output.cont(0) = 1.0
      for (i <- 0 until inputs.length)
        output.cont(0) *= inputs(i).cont(0)
    }
  }


  def copy(args: IndexedSeq[ArgumentType]) = new Product(args)

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


  import ml.wolfe.term.TermImplicits._

  def main(args: Array[String]) {

    class Gen[T](gen: => T) {
      def eval = gen
    }

    def rand = math.random

    val gen = new Gen(rand)

    println(gen.eval)
    println(gen.eval)


    //    val result1 = argmax(W){ w => loss(w)}

    //    val result = argmax(W){ w => loss2(w._1)}

    //
    //    val dom = doubles x seqs(doubles,2)
    //
    //    val x = dom.variable("x")
    //    val d:DoubleTerm = x._2(2)
    //
    //    implicit val vecs = vectors(2)
    //    val y = vecs.variable("y")
    //    val term = y dot vecs.Const(1.0,2.0)

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


class Iverson[T <: BoolTerm](val arg: T) extends UnaryTerm[T, DoubleDom] with ComposedDoubleTerm {
  def composer() = new Evaluator {
    def eval(inputs: Array[Setting], output: Setting) = {
      output.cont(0) = if (inputs(0).disc(0) == 0) 0.0 else 1.0
    }
  }

  def copy(args: IndexedSeq[ArgumentType]) = new Iverson(args(0))

  def differentiator(wrt: Seq[Var[Dom]]) = ???
}

trait BinaryDiscreteOperator[D <: Dom, A <: Dom] extends Composed[D] {

  self =>

  type ArgumentType = Term[A]

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

  def copy(args: IndexedSeq[ArgumentType]) = new BinaryDiscreteOperator[D, A] {

    val domain: D = self.domain

    def arg1 = args(0)
    def arg2 = args(1)
    def op(a1: Int, a2: Int) = self.op(a1, a2)
  }
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


