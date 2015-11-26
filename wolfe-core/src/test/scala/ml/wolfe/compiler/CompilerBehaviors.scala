package ml.wolfe.compiler

import ml.wolfe.Language._
import ml.wolfe._
import ml.wolfe.term.{WolfeSpec, termdef}
import org.nd4s.Implicits._
import ml.wolfe.compiler.nd4s.PimpMyND4S._
import org.nd4j.linalg.ops.transforms.{Transforms => num}

/**
  * @author riedel
  */
trait CompilerBehaviors extends {
  this: WolfeSpec =>

  def supportForwardActivation(newCompiler: => Compiler): Unit = {
    "support forward evaluation of matrix vector multiplication" in {
      val W = Var[Tensor]
      val x = Var[Tensor]
      val term = sigmoid(W * x)

      val module = newCompiler.compile(term)
      module.init(W := ones(2,2))
      module.forward(x := vec(1.0, 2.0).t)
      module.output() should equal (num.sigmoid(vec(3.0, 3.0)))
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
      module.init(params := Params(ones(2, 2), vec(1.0, 1.0).t))
      module.forward(input := Input(vec(1.0, 2.0).t))

      val expected = num.sigmoid(vec(3.0, 3.0) + vec(1.0, 1.0))
      module.output() should equal(expected)
    }
  }

  def supportBackwardPass(newCompiler: => Compiler): Unit = {
    "support backward evaluation of matrix vector multiplication" in {
      val W = Var[Tensor]
      val x = Var[Tensor]
      val term = sigmoid(W * x)

      val module = newCompiler.compile(term)
      module.init(W := ones(2, 2))
      module.forward(x := vec(1.0, 2.0).t)
      module.backward(vec(1.0, 1.0))

      val y_pre = ones(2,2) ** vec(1.0, 2.0).t
      val y = num.sigmoid(y_pre)
      val gradY = (-y + 1.0) :* y

      val expected = vec(1.0, 2.0) outer gradY

      module.gradient(W) should equal (expected)
    }
  }

  def supportMatrixFactorization(newCompiler: => Compiler) = {
    @termdef case class Theta(cols: Seq[Tensor], rows: Seq[Tensor])

    val theta = Var[Theta]
    val row = Var[Int]
    val col = Var[Int]
    val target = Var[Tensor] //double in {-1,1}
    val score = theta.rows(row) * theta.cols(col)
    val loss = log(sigmoid(score * target))

    def init = ones(2,1)
    def scalar(value: Double) = vec(value)

    "support the forward pass for a matrix factorization model" in {

      val module = newCompiler.compile(loss)
      module.init(theta := Theta(Seq(init, init), Seq(init.t, init.t)))
      module.forward(row := 0, col := 0, target := scalar(1))

      val expected = log(sigmoid(init.t dot init))
      module.output() should equal(expected)
    }

    "support the backward pass for a matrix factorization model" in {

      val module = newCompiler.compile(loss)
      module.init(theta := Theta(Seq(init, init), Seq(init.t, init.t)))
      module.forward(row := 0, col := 1, target := scalar(1.0))
      module.backward(scalar(1.0))

      val expected = vec(1 - num.sigmoid(vec(2))(0), 1 - num.sigmoid(vec(2))(0)) //fixme
      val result = module.gradient(theta)
      result.rows(0) should equal(expected)
      result.cols(1) should equal(expected.t)
    }

    "support parameter updates for a matrix factorization model" in {

      val module = newCompiler.compile(loss)
      module.init(theta := Theta(Seq(init, init), Seq(init.t, init.t)))
      module.forward(row := 0, col := 1, target := scalar(1.0))
      module.backward(scalar(1.0))
      module.updateParameters(1.0)

      val gradient = vec(1 - num.sigmoid(vec(2))(0), 1 - num.sigmoid(vec(2))(0)) //fixme
      val updated = init.t + gradient
      val result = module.param(theta)
      result.rows(0) should equal(updated)
      result.rows(1) should equal(init.t)
      result.cols(0) should equal(init)
      result.cols(1) should equal(updated.t)
    }
  }
}
