package ml.wolfe.neural

import breeze.linalg.{DenseVector, DenseMatrix}

/**
 * Created by narad on 11/24/14.
 */
class NeuralNetwork(val layers: Seq[NetworkLayer]) {
  require(layers.head.isInstanceOf[HiddenLayer], "Input layer must be of type HiddenLayer (for now.)")
  require(layers.last.isInstanceOf[OutputLayer], "Output layer must be of type OutputLayer.")

  for (i <- 0 until layers.size - 2) {
    val slice = layers.view(i, i + 1)
    println("Slice = " + slice.mkString(", "))
    require(slice(0).outputSize == slice(1).inputSize, "Layers %d and %d don't agree in output - input sizes".format(i, i+1))
  }

  def paramSize: Int = layers.foldLeft(0){ case(sum, l) => sum + l.W.rows * l.W.cols }

  def gradients: Seq[Double] = {
    layers.map(_.grads.toDenseVector.toArray).flatten
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
}
