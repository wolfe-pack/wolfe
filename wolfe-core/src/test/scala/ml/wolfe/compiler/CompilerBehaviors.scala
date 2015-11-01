package ml.wolfe.compiler

import breeze.linalg.DenseMatrix
import ml.wolfe.Language._
import ml.wolfe._
import ml.wolfe.term.termdef
import org.scalatest.{Matchers, WordSpec}

/**
 * @author riedel
 */
trait CompilerBehaviors extends {
  this: WordSpec with Matchers =>

  def supportForwardActivation(newCompiler: => Compiler): Unit = {
    "support forward evaluation of matrix vector multiplication" in {
      val W = Var[Tensor]
      val x = Var[Tensor]
      val term = sigmoid(W * x)

      val module = newCompiler.compile(term)
      module.init(W := DenseMatrix.ones(2, 2))
      module.forward(x := DenseMatrix(1.0, 2.0))
      module.output() should be(breeze.numerics.sigmoid(DenseMatrix(3.0, 3.0)))
    }
  }

  def supportForwardActivationWithComposedValues(newCompiler: => Compiler): Unit = {

    //todo: build better spec structure to avoid repetition in spec text
    "support forward evaluation of matrix vector multiplication with composed values" in {
      @termdef case class Params(W: Tensor, b: Tensor)
      @termdef case class Input(x: Tensor)
      val params = Var[Params]
      val input = Var[Input]
      val term = sigmoid(params.W * input.x + params.b)

      val module = newCompiler.compile(term)
      module.init(params := Params(DenseMatrix.ones(2, 2), DenseMatrix(1.0, 1.0)))
      module.forward(input := Input(DenseMatrix(1.0, 2.0)))
      module.output() should be(breeze.numerics.sigmoid(DenseMatrix(3.0, 3.0) + DenseMatrix(1.0, 1.0)))
    }

  }
  def supportBackwardPass(newCompiler: => Compiler): Unit = {

    "support backward evaluation of matrix vector multiplication" ignore {
      val W = Var[Tensor]
      val x = Var[Tensor]
      val term = sigmoid(W * x)

      val module = newCompiler.compile(term)
      module.init(W := ???)
      module.forward(x := ???)
      module.backward(???)
      module.gradient(W) should be(null)
      //module.gradient(x) should be ??? if x is cont.
    }

  }

}
