package ml.wolfe.term

import ml.wolfe.fg20._

/**
 * @author riedel
 */
trait Term[D <: Dom] {
  val domain: D
  def vars: Clique
  def evaluator(): Evaluator

  def apply(state: State): domain.Value = {
    val eval = evaluator()
    val output = domain.vars.createSetting()
    val input = vars.createPartialSetting(state)
    eval.eval(input, output)
    domain.settingToValue(output)
  }
}

trait Differentiable[D <: Dom] extends Term[D] {
  def differentiator() : Differentiator
}

trait Differentiator {
  def gradientAndValue(outputError:Setting, inputActivation:Setting, gradient:Setting, value:Setting)
}



class Constant[D <: Dom](val domain: D, val value: D#Value) extends Term[D] {
  val vars = new SimpleClique()
  val evaluator = new Evaluator {
    val result = domain.createPartialSetting(value.asInstanceOf[domain.Value])
    def eval(input: Setting, output: Setting) = {
      output := result
    }
  }
}

abstract class SeqDom[D <: Dom](val elementDom:D) extends Dom {
  type Value = IndexedSeq[elementDom.Value]
}

class SeqTerm[D <: Dom](val args: IndexedSeq[Term[D]]) extends Term[SeqDom[D]] {

  lazy val vars   = Clique.merge(args.map(_.vars))
  lazy val domain = ??? //IndexedSeqDomain()

  def evaluator() = new Evaluator {
    val arg2vars   = args.map(a => CliqueMapping(vars, a.vars)).toArray
    val argInputs  = args.map(_.vars.createSetting()).toArray
    val argOutputs = args.map(_.domain.vars.createSetting()).toArray
    val argEvals   = args.map(_.evaluator()).toArray
    def eval(input: Setting, output: Setting) = {
      for (i <- 0 until args.length) {
        arg2vars(i).copyForward(input, argInputs(i))
        argEvals(i).eval(argInputs(i), argOutputs(i))
      }
      Setting.merge(argOutputs, output)

    }
  }
}

trait Evaluator {
  def eval(input: Setting, output: Setting)
}

trait Fun[A <: Dom, B <: Dom] {
  val from: A
  val to  : B
  def apply(arg: from.Value): to.Value = {
    val input = from.createPartialSetting(arg)
    val output = to.createSetting()
    applier().apply(input, output)
    to.settingToValue(output)
  }
  def applier(): Applier
}

trait Applier {
  def apply(args: Setting, result: Setting)
}

class Parameter[D <: Dom](val domain: D)

trait LambdaAbstraction[A <: Dom, B <: Dom] extends Fun[A, B] {
  def body: Term[B]
  val param: Parameter[A]
  val to   = body.domain
  val from = param.domain
  //assert that free parameters of body are equal to parameter specified
  def applier() = new Applier {
    val bodyEval = body.evaluator()
    def apply(args: Setting, result: Setting) = {
      bodyEval.eval(args, result)
    }
  }
}

//trait Potential[A <: Domain[_]] extends Fun[A, AtomicDomain.Cont] {
//  val to = AtomicDomain.cont("result")
//}
//

trait FunApp[A <: Dom, B <: Dom] extends Term[B] {
  def fun: Fun[A, B]
  def arg: Term[A]

  def vars = arg.vars

  def evaluator() = new Evaluator {

    val argEval   = arg.evaluator()
    val applier   = fun.applier()
    val argOutput = arg.domain.createSetting()

    def eval(input: Setting, output: Setting) = {
      argEval.eval(input, argOutput)
      applier.apply(argOutput, output)
    }
  }

}

trait Potential[P <: Dom] extends Fun[P,DoubleDom] {
  val to = new DoubleDom
}

abstract class Max[MaxOver <: Dom, Params <: Dom](val fun: Potential2[MaxOver, Params]) extends Potential[Params] {
  val from: Params = fun.from.arg2
}

class Sum(val from: DoubleSeqDom) extends Potential[DoubleSeqDom] {

  def applier() = new Applier {
    def apply(args: Setting, result: Setting) = {
      var tmp = 0.0
      for (i <- 0 until args.cont.length) tmp += args.cont(i)
      result.cont(0) = tmp
    }
  }
}

abstract class Tuple2Domain[D1 <: Dom, D2 <: Dom] extends Dom {
  val arg1: D1
  val arg2: D2

  type Value = (arg1.Value,arg2.Value)
}

class DoubleDom extends Dom {
  type Value = Double
  private val variable: ContVar = new ContVar()
  val vars = new SimpleClique(contVars = Array(variable))
  def valueToState(value: Value) = SingletonState(variable,value)
  def stateToValue(state: State) = state(variable)
}


trait BackPropagator {
  def propagate(outputError: Setting, inputActivation: Setting, inputError: Setting)
}

trait Composer {
  def compose(inputs: Array[Setting], output: Setting)
}

