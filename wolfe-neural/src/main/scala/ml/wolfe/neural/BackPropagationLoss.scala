package ml.wolfe.neural

import ml.wolfe.FactorGraph._
import ml.wolfe.fg.Potential
import breeze.linalg.{DenseVector, DenseMatrix}


/**
 * Created by narad on 11/5/14.
 */
class BackPropagationLoss(edge: Edge, network: NeuralNetwork, input: DenseMatrix[Double], output: DenseVector[Double], rate: Double = 1.0) extends Potential  {
  println("Created BackProp Loss Factor.")
  val e = edge.msgs.asVector

  override def valueAndGradientForAllEdges(): Double = {
    if (e.n2f.exists(_ != 0.0)) {
      val updates = e.n2f.toArray
      var i = 0
      println("Update from Variable: " + e.n2f.mkString(", "))
      for (layer <- network.layers) {
//        println("layer = " + layer.size)
        layer.updateWithGradients(DenseMatrix(updates.slice(i, i+layer.size)).t)
        i += layer.size
      }
    }
    else {
      println("Null Gradients...")
    }
    network.backprop(input, output, updateWeights = false, rate = rate)
//    println("gradient = " + network.gradients.mkString(", "))
    e.f2n = network.gradients
    println("Update to Variable: " + e.f2n.mkString(", "))
   0.0
  }
}
















//object DNN extends App {
//
//  val numLayers = 5
//  val dims = Array(5, 20, 30, 10, 3)
//  (1 to numLayers) map { l =>
//    val w = DenseMatrix.fill[Double](1,5)(0.5)
//    val b = DenseVector.fill[Double](dims(l))(0.5)
//    if (l == numLayers) {
//      new OutputLayer(w, b, ActivationFunctions.sigmoid, ActivationFunctions.δ_sigmoid)
//    }
//    else {
//      new HiddenLayer(w, b, ActivationFunctions.sigmoid, ActivationFunctions.δ_sigmoid)
//    }
//  }
//
//}
//
//object ExampleDBN extends App {
//  val learningRate = 0.01
//  val l1 = 0.00
//  val l2 = 0.0001
//  val nEpochs = 1000
//  val dataPath = "/Users/narad/Downloads/mnist.pkl.gz"
//  val imagePath = "/Users/narad/Downloads/train-images-idx3-ubyte"
//  val labelPath = "/Users/narad/Downloads/train-labels-idx1-ubyte"
//  val bachSize = 20
//  val nHidden = 500
//
//  val data = new MNISTReader(imagePath, labelPath)
//  println(data.size)
//
//}




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
