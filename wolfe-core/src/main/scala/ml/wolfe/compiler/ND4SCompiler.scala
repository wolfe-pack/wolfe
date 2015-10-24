package ml.wolfe.compiler

import breeze.numerics.sigmoid
import ml.wolfe.Tensor
import ml.wolfe.term._

import scala.collection.mutable

/**
 * @author rockt
 */
object ND4SCompiler {


  def compile[T](term: Term[T]): Module[T] = {

    val var2InputBox = new mutable.HashMap[Var[Any],InputBox]()
    val var2ParamBox = new mutable.HashMap[Var[Any],ParamBox]()

    def compile[T](term: Term[T], paramBindings: Bindings): Box = {
      val box = term match {
        case v: Var[_] if paramBindings.contains(v) =>
          val result = new ParamBox(1)
          var2ParamBox(v) = result
          result
        case v: Var[_] =>
          val result = new InputBox(1)
          var2InputBox(v) = result
          result
        case Sigmoid(arg) =>
          val argBox = compile(arg, paramBindings)
          new SigmoidBox(argBox)
        case TensorMul(arg1, arg2) =>
          val arg1Box = compile(arg1, paramBindings)
          val arg2Box = compile(arg2, paramBindings)
          new TensorProductBox(arg1Box, arg2Box)
      }
      box
    }


    new Module[T] {

      var compiled:Box = null

      def gradient[G](param: Var[G]) = {
        val paramBox = var2ParamBox(param)
        paramBox.grad(0).asInstanceOf[G]
      }

      def init(bindings: Binding[Tensor]*) = {
        compiled = compile(term, Bindings(bindings: _*))
        for (binding <- bindings; box <- var2ParamBox.get(binding.variable)) {
          box.output(0) = binding.value
        }
      }

      def forward(bindings: Binding[Tensor]*) = {
        for (binding <- bindings; box <- var2InputBox.get(binding.variable)) {
          box.output(0) = binding.value
        }
        compiled.forward()
      }

      def output() = {
        compiled.output(0).asInstanceOf[T]
      }

      def backward(output: T) = {
        compiled.backward(Array(output.asInstanceOf[Tensor]))
      }
    }

  }

}

trait Module[T] {
  def gradient[G](param: Var[G]): G

  def backward(output: T)

  def output(): T

  def forward(bindings: Binding[Tensor]*)

  def init(bindings: Binding[Tensor]*)

}

trait Box {
  def forward()

  def backward(gradOutput: Array[Tensor])

  def output: Array[Tensor]

  def gradInputs: Array[Tensor]

}

class ParamBox(size: Int) extends Box {
  val output = Array.ofDim[Tensor](size)
  val grad = Array.ofDim[Tensor](size)
  val gradInputs = Array[Tensor]()

  def backward(gradOutput: Array[Tensor]) = {
    for (i <- gradOutput.indices) {
      grad(i) += gradOutput(i)
    }
  }

  def forward() = {

  }

  def update(learningRate: Double): Unit = {
    for (i <- grad.indices) {
      output(i) += (grad(i) * learningRate)
    }

  }

}

class InputBox(size: Int) extends Box {
  val output = Array.ofDim[Tensor](size)
  val gradInputs = Array[Tensor]()

  def forward() = {

  }

  def backward(gradOutput: Array[Tensor]) = {

  }


}

class SigmoidBox(input: Box) extends Box {
  val output = Array.ofDim[Tensor](1)
  val gradInputs = Array.ofDim[Tensor](1)

  def forward() = {
    input.forward()
    output(0) = sigmoid(input.output(0))
  }

  def backward(gradOutput: Array[Tensor]) = {
    val y = sigmoid(input.output(0))
    val oneMinusY = y :* (-1) + 1.0
    gradInputs(0) = (y :* oneMinusY) :* gradOutput(0)
  }
}

class TensorProductBox(arg1: Box, arg2: Box) extends Box {
  val output = Array.ofDim[Tensor](1)
  val gradInputs = Array.ofDim[Tensor](2)

  def forward() = {
    arg1.forward()
    arg2.forward()
    output(0) = arg1.output(0) * arg2.output(0)
  }

  def backward(gradOutput: Array[Tensor]) = {
    gradInputs(0) = gradOutput(0) * arg2.output(0).t
    gradInputs(1) = arg1.output(0) * gradOutput(0).t
  }
}