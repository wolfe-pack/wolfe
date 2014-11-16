package ml.wolfe.neural.io

import breeze.linalg.DenseMatrix
import java.io.{FileInputStream, DataInputStream}
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




/*
/**
* User: rockt
* Date: 11/20/13
* Time: 11:55 AM
*/

class Digit(val matrix: Tensor2, val label: Int) extends FactorieMatrix(matrix.dim1, matrix.dim2) { //FIXME: buggy since Matrix is empty
  override def apply(i: Int, j: Int) = matrix(i,j)
}

object MNIST {
  val testImageTensor = IDXReader.read("/MNIST/t10k-images.idx3-ubyte.zip") match { case t: FactorieTensor => t }
  val testLabelVector = IDXReader.read("/MNIST/t10k-labels.idx1-ubyte.zip") match { case v: FactorieVector => v }
  val mlpTest = IDXReader.read("/Users/narad/Downloads/mnist.pkl.gz") match { case t: FactorieTensor => t }
  val test: Seq[Digit] = testImageTensor.inners.view.zipWithIndex.map(t => new Digit(t._1, testLabelVector(t._2).toInt))
}


object MNISTDrawer extends SimpleSwingApplication {
  val images = MNIST.test
  val numExamples = 1000

  println("Drawing digits...")
  //def top = new MNISTFrame(images, 30, 50)
  def top = new MNISTFrame(images, 20, 10)
}

class MNISTFrame(digits: Seq[Digit], rows: Int, cols: Int) extends Frame {
  type Canvas = swing.Graphics2D
  title = "MNIST"
  val x = digits.head.dimensions(0)
  val y = digits.head.dimensions(1)
  preferredSize = (cols*x+12, rows*y+4)
  background = Color.BLACK
  contents = new Panel {
    override def paint(g: Canvas) = {
      g.setColor(Color.WHITE)
      for (i <- 0 until math.min(digits.size, cols*rows)) {
        val digit = digits(i)
        val xOffset = (i / cols) * x
        val yOffset = (i % cols) * y
        paintDigit(g, digit, xOffset, yOffset)
      }
    }
    def paintDigit(g: Canvas, digit: FactorieMatrix, xOffset: Int = 0, yOffset: Int = 0) = {
      g.setColor(Color.WHITE)
      for {
        row <- 0 until x
        col <- 0 until y
      } {
        val value = digit(row,col) / 255.0
        g.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER, value.toFloat))
        //g.drawRect(row + xOffset, col + yOffset, 1, 1)
        g.drawRect(col + yOffset, row + xOffset, 1, 1)
      }

      //draw a border for the digit
      for {
        row <- 0 until x
        col <- 0 until y
        if row == 0 || row == x-1 || col == 0 || col == y-1
      } {
        g.setColor(Color.BLUE)
        g.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER, 0.3f))
        g.drawRect(col + yOffset, row + xOffset, 1, 1)
      }
    }
  }
  //centerOnScreen()
  listenTo(this)
  reactions += {
    case WindowClosing(e) => {
      println("Exiting...")
      System.exit(0)
    }
  }
}

*/