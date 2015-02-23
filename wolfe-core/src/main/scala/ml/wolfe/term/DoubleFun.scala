package ml.wolfe.term

import ml.wolfe.util.Math._

/**
 * @author riedel
 */
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

class Sqrt[T <: Term[DoubleDom]](override val arg: T) extends DoubleFun(arg, math.sqrt, x => 0.5 / math.sqrt(x))

class Clip[T <:DoubleTerm](arg:T) extends DoubleFun(arg,x => if (x > 0.0) x else 0.0, x => if (x > 0.0) 1.0 else 0.0)

class Log[T <: DoubleTerm](override val arg: T) extends DoubleFun(arg, math.log, logDeriv)

class Tanh[T <: DoubleTerm](override val arg: T) extends DoubleFun(arg, tanh, tanhDeriv)
