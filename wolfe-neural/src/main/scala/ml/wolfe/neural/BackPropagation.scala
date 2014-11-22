package ml.wolfe.neural

import cc.factorie.la.DenseTensor1
import cc.factorie.optimize._
import ml.wolfe.{GradientBasedOptimizer, FactorGraph}
import ml.wolfe.FactorGraph._
import ml.wolfe.fg.{CellLogisticLoss, VectorMsgs, Potential}
import ml.wolfe.neural.io.MNISTReader
import breeze.linalg.{DenseVector, DenseMatrix}
import ml.wolfe.neural.math.ActivationFunctions
import ml.wolfe.util.{ProgressLogging, Timer}


/**
 * Created by narad on 11/5/14.
 */
class BackPropagationLoss(edge: Edge, network: MultiLayerPerceptron, input: DenseMatrix[Double], output: DenseVector[Double], rate: Double = 1.0) extends Potential  {
  println("Created BackProp Loss Factor.")
  val e = edge.msgs.asVector
  println(edge.n)
  println(edge.f)



  override def valueAndGradientForAllEdges(): Double = {
    println("f2n = " + e.f2n)
    println("n2f = " + e.n2f)
    if (e.n2f != null) {
      val updates = e.n2f.toArray
      var i = 0
      println("GG: " + e.n2f.mkString(", "))
      for (layer <- network.layers) {
        layer.updateWithGradients(DenseMatrix(updates.slice(i, i+layer.size)))
        i += layer.size
      }
    }
    else {
      println("Null Gradients...")
    }
    network.backprop(input, output, updateWeights = false, rate = rate)
    println("gradient = " + network.gradients.mkString(", "))
    e.f2n = network.gradients
    1.0
  }

}

object BackPropTest extends App {

//  3 Inputs x 4 Hidden = 12 weight params, 3 input params, and 4 bias params
  val mode = "WOLFE"
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
  val nn = new MultiLayerPerceptron(Array(layer1, layer2))
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
      GradientBasedOptimizer(fg,new OnlineTrainer(_, gradientOptimizer, bpIters, fg.factors.size - 1) with ProgressLogging
      )
    }
    println("Done after " + Timer.reportedVerbose("optimization"))

  }
  else {
    nn.backprop(inputs, outputs, iters = bpIters, updateWeights = false, rate = alpha, verbose = false)
  }
}

object DNN extends App {

  val numLayers = 5
  val dims = Array(5, 20, 30, 10, 3)
  (1 to numLayers) map { l =>
    val w = DenseMatrix.fill[Double](1,5)(0.5)
    val b = DenseVector.fill[Double](dims(l))(0.5)
    if (l == numLayers) {
      new OutputLayer(w, b, ActivationFunctions.sigmoid, ActivationFunctions.δ_sigmoid)
    }
    else {
      new HiddenLayer(w, b, ActivationFunctions.sigmoid, ActivationFunctions.δ_sigmoid)
    }
  }

}

object ExampleDBN extends App {
  val learningRate = 0.01
  val l1 = 0.00
  val l2 = 0.0001
  val nEpochs = 1000
  val dataPath = "/Users/narad/Downloads/mnist.pkl.gz"
  val imagePath = "/Users/narad/Downloads/train-images-idx3-ubyte"
  val labelPath = "/Users/narad/Downloads/train-labels-idx1-ubyte"
  val bachSize = 20
  val nHidden = 500

  val data = new MNISTReader(imagePath, labelPath)
  println(data.size)

}




//    def readMatrix(dim1: Int, dim2: Int): DenseMatrix = {
//      (0 until dim1) map (ix => readVector(dim2).toArray)
//      new DenseMatrix(dim1, dim2)
//    }
//
//    def readTensor(dim1: Int, dim2: Int, dim3: Int): DenseMatrix = {
//      val tensor = new FactorieTensor(dim1, dim2, dim3)
//      (0 until dim1) foreach (ix => tensor update (ix, readMatrix(dim2, dim3)))
//      tensor
//    }
//
//    rank match {
//      case 1 => readVector(dims(0))
//      case 2 => readMatrix(dims(0), dims(1))
//      case 3 => readTensor(dims(0), dims(1), dims(2))
//      case r => throw new IllegalStateException(s"I can't handle rank $r tensors")
//    }
//  }
