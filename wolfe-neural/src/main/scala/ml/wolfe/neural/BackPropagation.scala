package ml.wolfe.neural

import java.io.FileInputStream
import java.util.zip.GZIPInputStream

import ml.wolfe.fg.Potential
import ml.wolfe.neural.io.MNIST
import breeze.linalg.{DenseVector, DenseMatrix}
import ml.wolfe.neural.math.ActivationFunctions



/**
 * Created by narad on 11/5/14.
 */
class BackPropagation(network: MultiLayerPerceptron, input: DenseMatrix[Double], output: DenseMatrix[Double]) extends Potential  {

  def prop() = {
    for (layer <- network.layers) {
      layer match {
        case hl: HiddenLayer => hl.propagateForward(input)
      }
    }

  }

  override def valueAndGradientForAllEdges(): Double = {
    prop()
    1.0
  }

  override def mapF2N(): Unit = super.mapF2N()

}

object BackProp extends App {

  // 3 Inputs x 4 Hidden = 12 weight params, 3 input params, and 4 bias params
//  val inputs = DenseVector[Double](Array(1.0, 2.0, 3.0))
//  val w1 = DenseMatrix[Double, DenseVector[Double]](DenseVector(Array(0.0, 0.0, 0.0)),
//                       DenseVector(Array(0.0, 0.0, 0.0)))
//  val b1 = DenseVector(Array(0.0, 0.0, 0.0))
//  val layer1 = new HiddenLayer(w1, b1, ActivationFunctions.sigmoid)
//  val layer2 = new HiddenLayer(w1, b1, ActivationFunctions.sigmoid)
////  val layer3 = new OutputLayer()
//  val nn = new MultiLayerPerceptron(Array(l1, l2, l3))
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


import java.io.{DataInputStream, File, InputStream}
import scala.collection.mutable.ArrayBuffer

// Adapted to Scala from Gabe Johnson's code:
// https://code.google.com/p/pen-ui/source/browse/trunk/skrui/src/org/six11/skrui/charrec/MNISTReader.java

class MNISTReader(dataFile: String, labelFile: String) extends Iterable[(DenseMatrix[Double], Double)] {

  def iterator = {
    val labels = new DataInputStream(new FileInputStream(labelFile))
    val images = new DataInputStream(new FileInputStream(dataFile))
    val magicNumber1 = labels.readInt()
    if (magicNumber1 != 2049) {
      System.err.println("Label file has wrong magic number: " + magicNumber1 + " (should be 2049)")
      System.exit(0)
    }
    val magicNumber2 = images.readInt()
    if (magicNumber2 != 2051) {
      System.err.println("Image file has wrong magic number: " + magicNumber2 + " (should be 2051)")
      System.exit(0)
    }
    val numLabels = labels.readInt()
    val numImages = images.readInt()
    val numRows = images.readInt()
    val numCols = images.readInt()
    println("num labels = " + numLabels)
    println("num images = " + numImages)
    println("num rows = " + numRows)
    println("num cols = " + numCols)
    if (numLabels != numImages) {
      System.err.println("Image file and label file do not contain the same number of entries.")
      System.err.println("  Label file contains: " + numLabels)
      System.err.println("  Image file contains: " + numImages)
      System.exit(0)
    }

    val start = System.currentTimeMillis()
    var numLabelsRead = 0
    var numImagesRead = 0
    val pairs = new ArrayBuffer[(DenseMatrix[Double], Double)]
    while (labels.available() > 0 && numLabelsRead < numLabels) {
      val labelByte = labels.readByte()
      numLabelsRead += 1
      val image = Array.ofDim[Double](numCols, numRows)
      for (colIdx <- 0 until numCols) {
        for (rowIdx <- 0 until numRows) {
          image(colIdx)(rowIdx) = anyToDouble(images.readUnsignedByte())
        }
      }
      numImagesRead += 1
      val matrix = DenseMatrix(image.flatten)
     // println(image.map(_.mkString(", ")).mkString("\n"))
      pairs += ((matrix, anyToDouble(labelByte)))

      // At this point, 'label' and 'image' agree and you can do whatever you like with them.

      if (numLabelsRead % 10 == 0) {
        System.out.print(".")
      }
      if ((numLabelsRead % 800) == 0) {
        System.out.print(" " + numLabelsRead + " / " + numLabels)
        val end = System.currentTimeMillis()
        val elapsed = end - start
        val minutes = elapsed / (1000 * 60)
        val seconds = (elapsed / 1000) - (minutes * 60)
        System.out.println("  " + minutes + " m " + seconds + " s ")
      }
    }
    pairs.iterator
  }

  def anyToDouble(value: AnyVal): Double = value match {
    case b: Byte => b.toDouble
    case s: Short => s.toDouble
    case i: Int => i.toDouble
    case f: Float => f.toDouble
    case d: Double => d
  }
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
