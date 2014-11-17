package ml.wolfe.neural

import ml.wolfe.fg.Potential
import ml.wolfe.neural.io.MNISTReader
import breeze.linalg.{DenseVector, DenseMatrix}
import ml.wolfe.neural.math.ActivationFunctions



/**
 * Created by narad on 11/5/14.
 */
//class BackPropagation(network: MultiLayerPerceptron, input: DenseMatrix[Double], output: DenseMatrix[Double]) extends Potential  {
//
//  override def valueAndGradientForAllEdges(): Double = {
//    network.backprop(input)
//    1.0
//  }
//
//  override def mapF2N(): Unit = super.mapF2N()
//
//}

object BackProp extends App {

//  3 Inputs x 4 Hidden = 12 weight params, 3 input params, and 4 bias params
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
  val bpIters = 500
  nn.backprop(inputs, outputs, iters = bpIters, verbose = false)
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
