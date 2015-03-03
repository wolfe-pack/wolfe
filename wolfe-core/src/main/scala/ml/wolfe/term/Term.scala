package ml.wolfe.term

import cc.factorie.la.{SparseBinaryTensor1, SparseIndexedTensor1, DenseTensor1}
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

  abstract class AbstractEvaluator2(val input: Settings) extends ml.wolfe.term.Evaluator2


  def evaluatorImpl(in: Settings): Evaluator2 = ???

  abstract class AbstractDifferentiator2(val input: Settings,
                                         val error: Setting,
                                         val gradientAccumulator: Settings) extends ml.wolfe.term.Differentiator2

  class EmptyDifferentiator2(in: Settings, err: Setting, gradientAcc: Settings) extends AbstractDifferentiator2(in, err, gradientAcc) {
    val eval = evaluatorImpl(in)
    val output = eval.output

    def forward()(implicit execution: Execution): Unit = {
      eval.eval()(execution.copy(typ = Execution.EmptyDiff)) //ensures that log terms still render
    }

    def backward()(implicit execution: Execution) {}
  }

  def differentiator2(wrt: Seq[Var[Dom]])(in: Settings, err: Setting, gradientAcc: Settings): Differentiator2 =
    new EmptyDifferentiator2(in, err, gradientAcc)

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

  def argmaxerImpl(wrt: Seq[Var[Dom]])(observed: Settings, msgs: Msgs): Argmaxer2 = {
    //todo: this could be type safe, for example by adding the argmax method to the RichDoubleTerm
    if (!domain.isDouble) sys.error("Argmax only supported for real valued terms")
    else if (wrt.forall(_.domain.isDiscrete)) ??? else ???
  }

}


/**
 * Provides a range of convenience methods to access Term functionality in more type safe ways.
 * @tparam D
 */
trait TermHelper[+D <: Dom] {
  this: Term[D] =>

  def clientEvaluator() = new ClientEvaluator

  def clientDifferentiator[T <: Dom](wrt: Var[T]) = new ClientDifferentiator[T](wrt)

  class ClientEvaluator() {
    val input = createSettings()
    val eval = evaluatorImpl(input)
    var count = 0

    def eval(args: Any*): domain.Value = {
      for ((v, i) <- vars.zipWithIndex) v.domain.copyValue(args(i).asInstanceOf[v.domain.Value], input(i))
      eval.eval()(Execution(count))
      count += 1
      domain.toValue(eval.output)
    }
  }

  class ClientDifferentiator[T <: Dom](val wrt: Var[T]) {
    val input = createSettings()
    val error = domain.toSetting(domain.one)
    val gradient = createSettings()
    val diff = differentiator2(Seq(wrt))(input, error, gradient)
    val indexOfWrt = vars.indexOf(wrt)
    var count = 0

    def differentiate(args: Any*): wrt.domain.Value = {
      for ((v, i) <- vars.zipWithIndex) v.domain.copyValue(args(i).asInstanceOf[v.domain.Value], input(i))
      gradient.foreach(_.resetToZero())
      diff.differentiate()(Execution(count, Execution.Diff))
      count += 1
      wrt.domain.toValue(gradient(indexOfWrt))
    }

  }

  def maxMarginals[T <: Dom](target: Var[T], wrt: Var[T])(incoming: wrt.domain.Marginals)(args: Any*): target.domain.Marginals = {
    val maxMarger = maxMarginalizer(Seq(wrt), Seq(target))
    val observed = vars.filter(v => v != wrt && v != target)
    val observedSettings = Dom.createSettings(observed, args)
    val result = target.domain.createZeroMsg()
    val incomingMsgs = Array(wrt.domain.toMsgs(incoming))
    maxMarger.maxMarginals(observedSettings, incomingMsgs, Array(result))
    target.domain.toMarginals(result)
  }

  def argmax[V <: Dom](wrt: Var[V], args: Any*): wrt.domain.Value = {
    val am = argmaxer(Seq(wrt))
    val observed = vars.filter(_ != wrt)
    val observedSettings = for ((a, v) <- args zip observed) yield v.domain.toSetting(a.asInstanceOf[v.domain.Value])
    val result = wrt.domain.createSetting()
    am.argmax(observedSettings.toArray, observed.map(_.domain.createMsg()).toArray, Array(result))
    wrt.domain.toValue(result)
  }

  def eval(args: Any*): domain.Value = {
    val ev = evaluator()
    val output = domain.createSetting()
    val argSettings = for ((a, v) <- args zip vars) yield v.domain.toSetting(a.asInstanceOf[v.domain.Value])
    ev.eval(argSettings.toArray, output)
    domain.toValue(output)
  }

  def eval2(args: Any*): domain.Value = {
    val argSettings = createSettings(args)
    val ev = evaluatorImpl(argSettings)
    ev.eval()(Execution(0))
    domain.toValue(ev.output)
  }


  def gradient[V <: Dom](wrt: Var[V], args: Any*): wrt.domain.Value = {
    require(args.length == vars.length, s"You need as many arguments as there are free variables (${vars.length})")
    val indexOfWrt = vars.indexOf(wrt)
    gradient(args, wrt = Seq(wrt))(indexOfWrt).asInstanceOf[wrt.domain.Value]
  }

  def gradient2[V <: Dom](wrt: Var[V], args: Any*): wrt.domain.Value = {
    require(args.length == vars.length, s"You need as many arguments as there are free variables (${vars.length})")
    val gradient = createZeroSettings()
    val diff = differentiator2(Seq(wrt))(createSettings(args), domain.toSetting(domain.one), gradient)
    diff.differentiate()(Execution(0, Execution.Diff))
    val indexOfWrt = vars.indexOf(wrt)
    wrt.domain.toValue(gradient(indexOfWrt))
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

  def createVariableSettings() = vars.map(_.domain.createSetting()).toArray

  def createSettings(args: Seq[Any]) = {
    Settings.fromSeq((args zip vars) map { case (a, v) => v.domain.toSetting(a.asInstanceOf[v.domain.Value])})
  }

  def createSettings() = {
    Settings.fromSeq(vars.map(_.domain.createSetting()))
  }

  def createZeroSettings() = {
    Settings.fromSeq(vars.map(_.domain.createZeroSetting()))
  }


}

trait ProxyTerm[D <: Dom] extends Term[D] with NAry {
  def self: Term[D]

  val domain = self.domain

  def vars = self.vars


  type ArgumentType = Term[D]

  def arguments = IndexedSeq(self)

  def evaluator() = self.evaluator()


  override def evaluatorImpl(in: Settings) = self.evaluatorImpl(in)


  override def differentiator2(wrt: Seq[Var[Dom]])(in: Settings, err: Setting, gradientAcc: Settings) =
    self.differentiator2(wrt)(in, err, gradientAcc)

  def differentiator(wrt: Seq[Var[Dom]]) = self.differentiator(wrt)

  def atomsIterator = self.atomsIterator

  override def toString = self.toString

  override def equals(obj: scala.Any) = obj match {
    case t: ProxyTerm[_] =>
      t.self == self && t.getClass == getClass
    case _ => false
  }
}

trait OwnedTerm[T] extends ProxyTerm[TypedDom[T]] {

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
      ranges.addIntoIfChanged(error, gradient(0))
    }
  }

  def evaluator() = new Evaluator {

    def eval(inputs: Array[Setting], output: Setting) = {
      ranges.copy(inputs(0), output)
    }
  }


  override def evaluatorImpl(in: Settings) = new Evaluator2 {
    def eval()(implicit execution: Execution) = {}

    val input = in
    val output = in(0)
  }


  override def differentiator2(wrt: Seq[Var[Dom]])(in: Settings, err: Setting, gradientAcc: Settings) =
    new AbstractDifferentiator2(in, err, gradientAcc) {
      val output = in(0)
      val isWrt = wrt.contains(self)

      def forward()(implicit execution: Execution) {}

      def backward()(implicit execution: Execution): Unit = {
        if (isWrt) gradientAccumulator(0).addIfChanged(error)
      }
    }

  override def toString = name
}

trait Atom[+D <: Dom] extends Var[D] {
  def offset: Int

  def atomsIterator = Iterator(this)

  override def hashCode() = offset

  override def equals(obj: scala.Any) = obj match {
    case a: Atom[_] =>
      a.domain == domain &&
        (a.ownerOrSelf eq ownerOrSelf) &&
        a.offset == offset
    case _ => false
  }

  def projectValue(setting: Setting) {}

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

case class AtomIndices(disc: Array[Int], cont: Array[Int], vect: Array[Int], mats: Array[Int])

object Atoms {
  def fromIterator(iterator: Iterator[Atom[Dom]]) = {
    val cont = new mutable.HashSet[DoubleVar]()
    val vect = new mutable.HashSet[VectorVar]()
    val disc = new mutable.HashSet[DiscVar[Any]]()
    val mats = new mutable.HashSet[MatrixVar]()

    for (a <- iterator) a match {
      case c: DoubleVar => cont += c
      case v: VectorVar => vect += v
      case m: MatrixVar => mats += m
      case d: DiscVar[_] => disc += d.asInstanceOf[DiscVar[Any]]
    }
    Atoms(disc.toSeq, cont.toSeq, vect.toSeq, mats.toSeq)
  }
}


/**
 * An evaluator calculates a setting based on a list of argument settings.
 */
trait Evaluator {

  /**
   * Compute a setting based on a list of input settings.
   * @param inputs input settings.
   * @param output output setting (will be updated).
   */
  def eval(inputs: Array[Setting], output: Setting)

}

trait AtomicEvaluator {
  def eval(atomSetting: Setting, output: Setting)
}

object AtomicEvaluator {
  def apply(term: Term[Dom]) = {
    val evaluator = term.evaluator()
    val atoms = term.atoms
    new AtomicEvaluator {
      val inputs = term.vars.map(_.domain.createSetting()).toArray

      def eval(atomSetting: Setting, output: Setting) = {
        //for each atom find argument index and offset
        //fill setting accordingly
        //now evaluate
        evaluator.eval(inputs, output)
      }
    }

  }
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


class DotProduct[T1 <: VectorTerm, T2 <: VectorTerm](val arg1: T1, val arg2: T2) extends ComposedDoubleTerm {

  self =>

  type ArgumentType = VectorTerm

  val arguments = IndexedSeq(arg1, arg2)

  def copy(args: IndexedSeq[ArgumentType]) = new DotProduct(args(0), args(1))

  def composer() = new Evaluator {
    def eval(inputs: Array[Setting], output: Setting) = {
      output.cont(0) = inputs(0).vect(0) dot inputs(1).vect(0)
    }
  }


  override def composer2(args: Settings) = new Composer2(args) {
    def eval()(implicit execution: Execution) = {
      output.cont(0) = input(0).vect(0) dot input(1).vect(0)
    }
  }


  override def differentiator2(wrt: Seq[Var[Dom]])(in: Settings, err: Setting, gradientAcc: Settings) =
    new ComposedDifferentiator2(wrt, in, err, gradientAcc) {

      def localBackProp()(implicit execution: Execution) = {
        val scale = error.cont(0)

        if (arg1.vars.nonEmpty) argErrors(0).vect.set(0, argOutputs(1).vect(0), scale)
        if (arg2.vars.nonEmpty) argErrors(1).vect.set(0, argOutputs(0).vect(0), scale)

      }
    }

  def differentiator(wrt: Seq[Var[Dom]]) = new ComposedDifferentiator {

    def withRespectTo = wrt

    def localBackProp(argOutputs: Array[Setting], outError: Setting, gradient: Array[Setting]): Unit = {
      val scale = outError.cont(0)

      if (arg1.vars.nonEmpty) gradient(0).vect.set(0, argOutputs(1).vect(0), scale)
      if (arg2.vars.nonEmpty) gradient(1).vect.set(0, argOutputs(0).vect(0), scale)

      /*
        if (arg1.vars.size > 0) {
          gradient(0).vect(0) := 0.0
          gradient(0).vect(0) +=(argOutputs(1).vect(0), scale)
        }
        if (arg2.vars.size > 0) {
          gradient(1).vect(0) := 0.0
          gradient(1).vect(0) +=(argOutputs(0).vect(0), scale)
        }
      */
    }
  }
}

class SparseL2[T1 <: VectorTerm, T2 <: VectorTerm](val arg: T1, val mask: T2 = null) extends ComposedDoubleTerm {

  self =>

  type ArgumentType = VectorTerm

  val arguments = if (mask == null) IndexedSeq(arg) else IndexedSeq(arg, mask)

  def copy(args: IndexedSeq[ArgumentType]) = new DotProduct(args(0), args(1))

  def composer() = new Evaluator {
    def eval(inputs: Array[Setting], output: Setting) = {
      val w = inputs(0).vect(0)
      if (mask != null) {
        val f = inputs(1).vect(0)
        output.cont(0) = 0.0
        f.foreachActiveElement { case (i, v) =>
          output.cont(0) += w(i) * w(i) * v
        }
      } else {
        output.cont(0) = w dot w
      }
      //      if (output.cont(0) != 0.0) {
      //        println("Huh?")
      //      }
      //      println("Eval: " + output.cont(0))
    }
  }

  def differentiator(wrt: Seq[Var[Dom]]) = new ComposedDifferentiator {

    def withRespectTo = wrt

    def localBackProp(argOutputs: Array[Setting], outError: Setting, gradient: Array[Setting]): Unit = {
      val scale = outError.cont(0)

      /*
      gradient(0).setVect(0, argOutputs(1).vect(0), scale)
      gradient(1).setVect(0, argOutputs(0).vect(0), scale)
      */

      val w = argOutputs(0).vect(0)

      if (mask != null) {
        val f = argOutputs(1).vect(0)
        import ml.wolfe.util.PimpMyFactorie._
        //need to multiply w with f
        gradient(0).vect.update(0, f)
        gradient(0).vect(0) :* w
        gradient(0).vect(0) *= 2.0 * scale
      } else {
        gradient(0).vect.update(0, w)
        gradient(0).vect(0) *= scale * 2.0
      }
      //todo: calculate gradient for mask!
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

class VectorConcatenation[T1 <: VectorTerm, T2 <: VectorTerm](val arg1: T1, val arg2: T2) extends Composed[GenericVectorDom] {

  self =>


  type ArgumentType = VectorTerm

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


  override def composer2(args: Settings) = new Composer2(args) {
    def eval()(implicit execution: Execution) = {
      output.vect(0) = input(0).mats(0) * input(1).vect(0)
    }
  }


  override def differentiator2(wrt: Seq[Var[Dom]])(in: Settings, err: Setting, gradientAcc: Settings) =
    new ComposedDifferentiator2(wrt, in, err, gradientAcc) {

      def localBackProp()(implicit execution: Execution) = {
        val A = argOutputs(0).mats(0)
        val x = argOutputs(1).vect(0)
        val errorVec = error.vect(0)
        require(A.dim2 == x.dim1, s"dimensions don't match: ${A.toDimensionsString} * ${x.dim1}")
        require(A.dim1 == errorVec.dim1, s"dimensions don't match: ${A.toDimensionsString} * ${x.dim1} => ${errorVec.dim1}")

        argErrors(0).mats(0) := 0.0
        argErrors(1).vect(0) := 0.0
        argErrors(0).mats(0) += errorVec outer x
        argErrors(1).vect(0) += A.t * errorVec

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

class Div[T1 <: DoubleTerm, T2 <: DoubleTerm](val arg1: T1, val arg2: T2) extends ComposedDoubleTerm {

  self =>

  type ArgumentType = DoubleTerm

  val arguments = IndexedSeq(arg1, arg2)

  def copy(args: IndexedSeq[ArgumentType]) = new Div(args(0), args(1))

  def composer() = new Evaluator {
    def eval(inputs: Array[Setting], output: Setting) = {
      output.cont(0) = inputs(0).cont(0) / inputs(1).cont(0)
    }
  }


  override def composer2(args: Settings) = new Composer2(args) {
    def eval()(implicit execution: Execution) = {
      output.cont(0) = input(0).cont(0) / input(1).cont(0)
    }
  }

  def differentiator(wrt: Seq[Var[Dom]]) = new ComposedDifferentiator {

    def withRespectTo = wrt

    def localBackProp(argOutputs: Array[Setting], outError: Setting, gradient: Array[Setting]): Unit = {
      val scale = outError.cont(0)

      val x = argOutputs(0).cont(0)
      val y = argOutputs(1).cont(0)

      gradient(0).cont(0) = scale / y
      gradient(1).cont(0) = -(scale * x) / (y * y)
    }
  }

  override def differentiator2(wrt: Seq[Var[Dom]])(in: Settings, err: Setting, gradientAcc: Settings) =
    new ComposedDifferentiator2(wrt, in, err, gradientAcc) {
      def localBackProp()(implicit execution: Execution) = {
        val scale = error.cont(0)

        val x = argOutputs(0).cont(0)
        val y = argOutputs(1).cont(0)

        argErrors(0).cont(0) = scale / y
        argErrors(1).cont(0) = -(scale * x) / (y * y)
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


case class Product(arguments: IndexedSeq[DoubleTerm]) extends ComposedDoubleTerm {

  type ArgumentType = DoubleTerm

  def composer() = new Evaluator {
    def eval(inputs: Array[Setting], output: Setting) = {
      output.cont(0) = 1.0
      for (i <- 0 until inputs.length)
        output.cont(0) *= inputs(i).cont(0)
    }
  }


  override def composer2(args: Settings) = new Composer2(args) {
    def eval()(implicit execution: Execution) = {
      output.cont(0) = 1.0
      for (i <- 0 until input.length)
        output.cont(0) *= input(i).cont(0)
    }
  }


  override def differentiator2(wrt: Seq[Var[Dom]])(in: Settings, err: Setting, gradientAcc: Settings) =
    new ComposedDifferentiator2(wrt, in, err, gradientAcc) {
      def localBackProp()(implicit execution: Execution) = {
        var total = error.cont(0)
        var zeros = 0
        var zeroIndex = -1
        for (i <- 0 until argOutputs.length) {
          //set to 0.0 by default
          argErrors(i) := 0.0
          if (argOutputs(i).cont(0) == 0.0) {
            zeros += 1
            zeroIndex = i
          } else
            total *= argOutputs(i).cont(0)
        }
        if (zeros == 1) {
          argErrors(zeroIndex).cont(0) = total
        } else if (zeros == 0) {
          for (i <- 0 until argOutputs.length)
            argErrors(i).cont(0) = total / argOutputs(i).cont(0)
        }
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


  override def composer2(args: Settings) = new Composer2(args) {
    def eval()(implicit execution: Execution) = {
      output.cont(0) = if (input(0).disc(0) == 0) 0.0 else 1.0
    }
  }

  def copy(args: IndexedSeq[ArgumentType]) = new Iverson(args(0))

  def differentiator(wrt: Seq[Var[Dom]]) = ???
}

class IntToDouble[T <: IntTerm](val int: T) extends ComposedDoubleTerm {
  type ArgumentType = T

  def arguments = IndexedSeq(int)

  def copy(args: IndexedSeq[ArgumentType]) = new IntToDouble(args(0))

  def composer() = new Evaluator {
    def eval(inputs: Array[Setting], output: Setting) = {
      output.cont(0) = inputs(0).disc(0)
    }
  }

  override def composer2(args: Settings) = new Composer2(args) {
    def eval()(implicit execution: Execution) = {
      output.cont(0) = input(0).disc(0)
    }
  }


  def differentiator(wrt: Seq[Var[Dom]]) = ???
}

case class Identity[D<:Dom, T <: Term[D]](self: T) extends ProxyTerm[D] {
  def copy(args: IndexedSeq[ArgumentType]) = new Identity[D,T](args(0).asInstanceOf[T])
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


  override def composer2(args: Settings) = new Composer2(args) {
    def eval()(implicit execution: Execution) = {
      output.disc(0) = op(input(0).disc(0), input(1).disc(0))
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


class DiscreteEquals[T](var arg1: DiscreteTerm[T], var arg2: DiscreteTerm[T]) extends BinaryDiscreteOperator[BoolDom, GenericDiscreteDom[T]] {
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


