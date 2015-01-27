package ml.wolfe.fg20

import cc.factorie.la.DenseTensor1

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.math._



/**
 * A neural unit is a deterministic potential which enforces a set of output variables to be
 * the result of some function applied to input values and parameters.
 * @author riedel
 */
trait NeuralUnit extends Potential {

  /**
   * The set of input variables (or "neurons" here)
   * @return input neurons.
   */
  def input: Clique

  /**
   * The set of output variables (or "neurons" here)
   * @return output neurons.
   */
  def output: Clique

  /**
   * The set of parameter variables.
   * @return parameter variables.
   */
  def params: Clique


  /**
   * The propagator defines how the unit propagates input activation and parameters to output activation.
   * This is the main method different types of units need to implement to define their behaviour.
   * @return a propagator.
   */
  def propagator(): Propagator

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


  def scorer(): Scorer = new Scorer {
    val propa          = propagator()
    val inputSetting   = input.createSetting()
    val expectedOutput = output.createSetting()
    val actualOutput   = output.createSetting()
    val paramsSetting  = params.createSetting()

    def score(setting: Setting): Double = {
      inputSetting.copyFrom(setting, inputMap)
      paramsSetting.copyFrom(setting, paramsMap)
      expectedOutput.copyFrom(setting, outputMap)
      propa.forward(inputSetting, paramsSetting, actualOutput)
      if (actualOutput.epsEquals(0.0001, expectedOutput)) 0.0 else Double.NegativeInfinity
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


trait NeuralNetwork extends NeuralUnit {
  def units: Seq[NeuralUnit]

  lazy val params = new SimpleClique(
    units.flatMap(_.params.discVars).distinct.toArray,
    units.flatMap(_.params.contVars).distinct.toArray,
    units.flatMap(_.params.vectVars).distinct.toArray)


  val paramVar2Units  = units.flatMap(u => u.params.vars.map(_ -> u)).groupBy(_._1).mapValues(_.map(_._2))
  val inputVar2Units  = units.flatMap(u => u.input.vars.map(_ -> u)).groupBy(_._1).mapValues(_.map(_._2))
  val outputVar2Units = units.flatMap(u => u.output.vars.map(_ -> u)).groupBy(_._1).mapValues(_.map(_._2))

  val sources = vars.filter(v => !outputVar2Units.contains(v) && !paramVar2Units.contains(v)).toSeq
  val sinks   = vars.filter(v => !inputVar2Units.contains(v) && !paramVar2Units.contains(v)).toSeq

  val input  = Clique.create(sources)
  val output = Clique.create(sinks)

  def propagator() = new Propagator {
    val dag = new DAG

    def forward(input: Setting, params: Setting, output: Setting): Unit = {
      //map input to the input cliques of the sources
      //then pass messages on edges upwards
    }
    def backward(inputActivation: Setting, outputError: Setting, paramsGradient: Setting, inputError: Setting): Unit = {

    }
  }


  //build DAG, find sources (inputs) and sinks (outputs)
  class DAG {

    class Node(val unit: NeuralUnit) {

      val parents  = new ArrayBuffer[(Node, CliqueMapping)]
      val children = new ArrayBuffer[(Node, CliqueMapping)]

      val outActivation = unit.output.createSetting()
      val outError      = unit.output.createSetting()

      val inActivation = unit.input.createSetting()
      val inError      = unit.input.createSetting()

      val paramsSetting  = unit.params.createSetting()
      val paramsGradient = unit.params.createSetting()

      def propagateActivationToParents(): Unit = {
        //for each output activation set all parent input cliques
        for ((parent, mapping) <- parents) mapping.copyForward(outActivation, parent.inActivation)
      }
    }

    val nodes = units.map(u => u -> new Node(u)).toMap
    for (u <- units; n = nodes(u)) {
      //find outgoing units
      n.parents ++= u.output.vars.flatMap(v => inputVar2Units(v)).toSeq.distinct.map(
        p => nodes(p) -> CliqueMapping(u.output,p.input) )
      n.children ++= u.input.vars.flatMap(v => outputVar2Units(v)).toSeq.distinct.map(
        c => nodes(c) -> CliqueMapping(u.input, c.output)
      )
    }



  }


}

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

  val input  = new SimpleClique(vectVars = Array(in))
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
