package ml.wolfe.neural

import breeze.linalg.{DenseVector, DenseMatrix}
import ml.wolfe.neural.math.ActivationFunctions

/**
 * Created by mbosnjak on 11/6/14.
 */

// extends iterable of layers ? (check of last layer)
// last layer needs to be non-hidden ?
// parameters: sequence of layers  +  final layer ?
class MultiLayerPerceptron(val layers: Seq[NetworkLayer]) {
  require(layers.head.isInstanceOf[HiddenLayer], "Input layer must be of type HiddenLayer (for now.)")
  require(layers.last.isInstanceOf[OutputLayer], "Output layer must be of type OutputLayer.")

  for (i <- 0 until layers.size - 2) {
    val slice = layers.view(i, i + 1)
    println("Slice = " + slice.mkString(", "))
    require(slice(0).outputSize == slice(1).inputSize, "Layers %d and %d don't agree in output - input sizes".format(i, i+1))
  }

  def predict(input: DenseMatrix[Double]): DenseVector[Double] = {
    var cmat = input
    for (l <- layers) {

    }
    ???
  }

  def paramSize: Int = layers.foldLeft(0){ case(sum, l) => sum + l.W.rows * l.W.cols }

  def gradients: Seq[Double] = {
//    layers.foreach{ l => println("g:\n" + l.grads + "\n") }
    layers.map(_.grads.toDenseVector.toArray).toSeq.flatten
  }

  def backprop(input: DenseMatrix[Double], outputs: DenseVector[Double], updateWeights: Boolean = true, iters: Int = 1, rate: Double = 0.9, verbose: Boolean = false) = {
    if (verbose) println("Performing backprop...")
    for (i <- 1 to iters) {
      if (verbose) println("Forward pass...")
      var lastOut = input
      for (layer <- layers) {
        lastOut = layer.output(lastOut, verbose=verbose)
      }
      println(lastOut)
      println(outputs.toDenseMatrix)
      println

      if (verbose) println("Backward pass...")
      var louts = outputs.toDenseMatrix
      var lastw = null.asInstanceOf[DenseMatrix[Double]]
      for (layer <- layers.reverse) {
        layer match {
          case x: OutputLayer => {
            louts = x.gradient(louts, verbose=verbose)
            lastw = x.W
          }
          case y: HiddenLayer => y.gradient(louts, lastw, verbose=verbose)
        }
      }
      if (verbose) println("Updating network weights...")
      if (updateWeights) {
        for (layer <- layers) layer.updateWithCachedGradients(eta = rate, verbose=verbose)
      }
    }
  }

  // need to define a total cost, output + regularization?
}

// object MultiLayerPerceptronPlayground extends App {   }



//
//  val x = DenseMatrix((0.1, 0.1), (0.2,0.1), (0.3, 0.5))
//
//  val W1 = DenseMatrix((1.0, 2.0, 3.0),(2.0, 2.0, 3.0))
//  val b1 = DenseVector(2.0, 2.0, 6.0)
//
//  val W2 = DenseMatrix((1.0, 1.0), (2.0, 2.0), (2.0, 1.0))
//  val b2 = DenseVector(3.0, 1.0)
//
//  val W3 = DenseMatrix((1.0, 3.0), (2.0, 2.0))
//  val b3 = DenseVector(1.0, 1.0)
//
//

//
//
//  val l1 = new HiddenLayer(x, 3, W1, b1, ActivationFunctions.sigmoid)
//
//  val l2 = new HiddenLayer(l1.output, 2, W2, b2, ActivationFunctions.sigmoid)
//  val l3 = new LogisticRegression(l2.output, W3, b3)
//
//
//  val mlp = new MultiLayerPerceptron(Seq(l1, l2, l3))
//
//
//  println("Hello MultiLayer Perceptron!")




/*
  def backprop(input: DenseMatrix[Double]) = {
    println("Performing backprop...")
    var lastOut = input
    for (layer <- layers) {
      layer match {
        case h: HiddenLayer => h.output(lastOut) //  hl.propagateForward(input)
      }
    }
  }
 */