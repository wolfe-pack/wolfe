package ml.wolfe.compiler

import ml.wolfe.Language._
import ml.wolfe._
import ml.wolfe.term.{WolfeSpec, termdef}
import org.nd4s.Implicits._
import org.nd4j.linalg.ops.transforms.Transforms._
import ml.wolfe.compiler.nd4s.PimpMyND4S._

/**
 * @author riedel
 */
trait CompilerBehaviors extends {
  this: WolfeSpec =>

  def supportForwardActivation(newCompiler: => Compiler): Unit = {
    "support forward evaluation of matrix vector multiplication" in {
      val W = Var[Tensor]
      val x = Var[Tensor]
      val term = sigm(W * x)

      val module = newCompiler.compile(term)
      module.init(W := ones(2,2))
      module.forward(x := vec(1.0, 2.0).t)

      module.output() should equal (sigmoid(vec(3.0, 3.0)))
    }
  }

  def supportForwardActivationWithComposedValues(newCompiler: => Compiler): Unit = {
    //todo: build better spec structure to avoid repetition in spec text
    "support forward evaluation of matrix vector multiplication with composed values" in {
      @termdef case class Params(W: Tensor, b: Tensor)
      @termdef case class Input(x: Tensor)
      val params = Var[Params]
      val input = Var[Input]
      val term = sigm(params.W * input.x + params.b)

      val module = newCompiler.compile(term)
      module.init(params := Params(ones(2, 2), vec(1.0, 1.0).t))
      module.forward(input := Input(vec(1.0, 2.0).t))

      val expected = sigmoid(vec(3.0, 3.0) + vec(1.0, 1.0))
      module.output() should equal(expected)
    }
  }

  def supportBackwardPass(newCompiler: => Compiler): Unit = {
    "support backward evaluation of matrix vector multiplication" in {
      val W = Var[Tensor]
      val x = Var[Tensor]
      val term = sigm(W * x)

      val module = newCompiler.compile(term)
      module.init(W := ones(2, 2))
      module.forward(x := vec(1.0, 2.0).t)
      module.backward(vec(1.0, 1.0))

      val y_pre = ones(2,2) * vec(1.0, 2.0).t
      val y = sigmoid(y_pre)
      val gradY = (-y + 1.0) :* y

      val expected = vec(1.0, 2.0) * gradY.t
      module.gradient(W) should equal (expected)
    }

  }

}
