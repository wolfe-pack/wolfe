package ml.wolfe.neural

import cc.factorie.la.DenseTensor1
import cc.factorie.optimize._
import ml.wolfe.{GradientBasedOptimizer, FactorGraph}
import ml.wolfe.fg.VectorMsgs
import ml.wolfe.neural.math.ActivationFunctions
import ml.wolfe.util.{ProgressLogging, Timer}
import breeze.linalg.{DenseVector, DenseMatrix}

/**
 * Created by narad on 11/24/14.
 */
object BackPropTest extends App {

  //  3 Inputs x 4 Hidden = 12 weight params, 3 input params, and 4 bias params
  val mode = "NOT_WOLFE"
  val optimizer = "SGD"
  val alpha = 0.9
  val inputs = DenseMatrix((1.0), (2.0), (3.0))
  val w1 = DenseMatrix((0.1, 0.2, 0.3, 0.4),
    (0.5, 0.6, 0.7, 0.8),
    (0.9, 1.0, 1.1, 1.2))
  val b1 = DenseVector(-2.0, -6.0, -1.0, -7.0)
  val w2 = DenseMatrix((1.3, 1.4),
    (1.5, 1.6),
    (1.7, 1.8),
    (1.9, 2.0))
  val b2 = DenseVector(-2.5, -5.0)
  val outputs = DenseVector(-0.8500, 0.7500)
  val layer1 = new HiddenLayer(w1, b1, ActivationFunctions.sigmoid, ActivationFunctions.δ_sigmoid)
  val layer2 = new OutputLayer(w2, b2, ActivationFunctions.tanh, ActivationFunctions.δ_tanh)
  val nn = new NeuralNetwork(Array(layer1, layer2))
  val bpIters = 50

  if (mode == "WOLFE") {
    val fg = new FactorGraph
    val paramSize = nn.paramSize
    println("Wrapping Neural Network with %d parameters.".format(paramSize))
    val v = fg.addVectorNode(dim = paramSize)
    v.variable.asVector.b = new DenseTensor1(Array.ofDim[Double](paramSize))
    fg.buildFactor(Seq(v))(_ map (_ => new VectorMsgs)) { e =>
      new BackPropagationLoss(e.head, nn, inputs, outputs)
    }
    println("Graph = " + fg.toInspectionString)

    val gradientOptimizer = optimizer match {
      case "SGD" => new ConstantLearningRate(baseRate = alpha)
      case "AdaGrad" => new AdaGrad(rate = alpha)
      case "AdaMira" => new AdaMira(rate = alpha)
      case "LBFGS" => new LBFGS(Double.MaxValue, Int.MaxValue) //rockt: not working atm
      case "AvgPerceptron" => new AveragedPerceptron()
    }

    fg.build()

    Timer.time("optimization") {
      GradientBasedOptimizer(fg, new OnlineTrainer(_, gradientOptimizer, bpIters, fg.factors.size - 1) with ProgressLogging
      )
    }
    println("Done after " + Timer.reportedVerbose("optimization"))

  }
  else {
    nn.backprop(inputs, outputs, iters = bpIters, updateWeights = true, rate = alpha, verbose = false)
  }
}