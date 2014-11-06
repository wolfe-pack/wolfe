package ml.wolfe.neural.io

import cc.factorie.la.{Tensor => TensorTrait, Tensor2}

import swing.Swing._
import swing._
import scala.swing.event.WindowClosing
import java.awt.{AlphaComposite, Color}
import ml.wolfe.neural.{FactorieVector, FactorieTensor, FactorieMatrix}

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