package ml.wolfe.fg20

import cc.factorie.la.DenseTensor1

import scala.math._

/**
 * A neural unit is a deterministic potential which enforces a set of output variables to be
 * the result of some function applied to input values and parameters.
 * @author riedel
 */
trait NeuralUnit extends Potential {

  def input: Clique

  def output: Clique

  def params: Clique

  lazy val discVars = input.discVars ++ params.discVars ++ output.discVars
  lazy val contVars = input.contVars ++ params.contVars ++ output.contVars
  lazy val vectVars = input.vectVars ++ params.vectVars ++ output.vectVars

  private lazy val inputMap =
    ArgMap.offset(input, 0, 0, 0)

  private lazy val paramsMap =
    ArgMap.offset(output, input.discVars.size, input.contVars.size, input.vectVars.size)

  private lazy val outputMap = ArgMap.offset(output,
    input.discVars.size + params.discVars.size,
    input.contVars.size + params.contVars.size,
    input.vectVars.size + params.vectVars.size)


  def propagator(): Propagator

  def scorer(): Scorer = new Scorer {
    val propa = propagator()
    val inputSetting = input.createSetting()
    val expectedOutput = output.createSetting()
    val actualOutput = output.createSetting()
    val paramsSetting = params.createSetting()

    def score(setting: Setting): Double = {
      inputSetting.copyFrom(setting, inputMap)
      paramsSetting.copyFrom(setting, paramsMap)
      expectedOutput.copyFrom(setting, outputMap)
      propa.forward(inputSetting,paramsSetting,actualOutput)
      if (actualOutput.epsEquals(0.0001,expectedOutput)) 0.0 else Double.NegativeInfinity
    }
  }
}


/**
 * A propagator calculate a forward message conditioned on input and parameters, and a backward message
 * conditioned on output error and input activation.
 */
trait Propagator {
  def forward(input: Setting, params: Setting, output: Setting)

  def backward(inputActivation: Setting, outputError: Setting, paramsGradient: Setting, inputError: Setting)
}


//trait NeuralNetwork extends NeuralUnit with Sum[NeuralUnit] {
//
//}

trait NeuralLoss extends Differentiable {
  def unit: NeuralUnit

  def loss: Differentiable //takes unit's output and calculates a loss/reward from it
}

abstract class NeuralL2Loss extends NeuralLoss {

}

object NeuralL2Loss {
  def apply(unit: NeuralUnit): NeuralL2Loss = ???
}


class SigmoidUnit(val in: VectVar, val out: VectVar) extends NeuralUnit {

  import ml.wolfe.fg20.SigmoidUnit._

  val input = new SimpleClique(vectVars = Array(in))
  val output = new SimpleClique(vectVars = Array(out))

  def params = Clique.empty

  def propagator(): Propagator = new Propagator {
    def forward(input: Setting, params: Setting, output: Setting): Unit = {
      if (output.vect(0) == null) output.vect(0) = new DenseTensor1(out.dim)
      for (i <- 0 until out.dim) output.vect(0)(i) = sigmoid(input.vect(0)(i))
    }

    def backward(inputActivation: Setting, outputError: Setting, paramsGradient: Setting, inputError: Setting): Unit = {
      //inputError = get gradient of activation function at current activation, and multiply with outputError
      //todo: should be hadamard product eventually, calling out GPU etc.
      for (i <- 0 until outputError.vect(0).size) {
        val gradientAtActivation = sigmoidDerivative(inputActivation.vect(0)(i))
        inputError.vect(0)(i) = outputError.vect(0)(i) * gradientAtActivation
      }
    }
  }

}

object SigmoidUnit {
  def sigmoid(x: Double) = 1.0 / (1.0 + exp(-x))

  def sigmoidDerivative(x: Double) = {
    val tmp = 1 + exp(x)
    exp(x) / (tmp * tmp)
  }

  def apply(in: NeuralUnit): NeuralUnit = ???
}

abstract class LinearTransformation(in: VectVar, weights: VectVar, bias: VectVar, out: VectVar) extends NeuralUnit
