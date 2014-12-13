package ml.wolfe.neural

import breeze.linalg.{DenseVector, DenseMatrix}
import scala.collection.mutable.ArrayBuffer

/**
 * Created by narad on 11/24/14.
 */
class NeuralNetwork { // (val layers: Seq[NetworkLayer]) {
  val layers = new ArrayBuffer[NetworkLayer]

  def addHiddenLayer(w: DenseMatrix[Double], b: DenseVector[Double], a: ActivationFunction) = {
    layers += new HiddenLayer(w, b, a, (layers.size, this))
  }

  def addOutputLayer(w: DenseMatrix[Double], b: DenseVector[Double], a: ActivationFunction) = {
    layers += new OutputLayer(w, b, a, (layers.size, this))
  }

  def paramSize: Int = layers.foldLeft(0){ case(sum, l) => sum + l.W.rows * l.W.cols }

  def gradients: Seq[Double] = {
    layers.map(_.grads.toDenseVector.toArray).flatten
  }

  def backprop(input: DenseMatrix[Double], outputs: DenseVector[Double], loss: NeuralLossFunction, updateWeights: Boolean = true, iters: Int = 1, rate: Double = 0.9, verbose: Boolean = false): Double = {
    if (verbose) println("Performing backprop...")
    var closs = -1.0
    for (i <- 1 to iters) {
      if (verbose) println("Forward pass...")
      var lastOut = input
      for (layer <- layers) {
        lastOut = layer.output(lastOut, verbose=verbose)
      }
      println(lastOut)

      println("Desired outs = [%s]".format(outputs.toDenseVector.toArray.toList.mkString(", ")))
      println("Current outs = [%s]".format(lastOut.toDenseVector.toArray.toList.mkString(", ")))

      val la = layers.last.activation
      println("Diffs = [%s]".format((outputs.toDenseMatrix - layers.last.theta.map(la.f).toDenseMatrix).toDenseVector.toArray.toList.mkString(", ")))

      closs = (outputs.toDenseMatrix - layers.last.theta.map(la.f).toDenseMatrix).toDenseVector.map(scala.math.pow(_, 2)).foldLeft(0.0)(_+_)  //loss(layers.last, outputs.toDenseMatrix).toDenseVector.foldLeft(0.0)(_+_)
      println("Current loss = " + closs)
      println

      if (verbose) println("Backward pass...")
      var louts = outputs.toDenseMatrix
//      var lastw = null.asInstanceOf[DenseMatrix[Double]]
      for (layer <- layers.reverse) louts = layer.gradient(louts, loss, verbose=verbose)

      println("grads = " + gradients.mkString(", "))

      if (updateWeights) {
        if (verbose) println("Updating network weights...")
        for (layer <- layers) layer.updateWithCachedGradients(eta = rate, verbose=verbose)
      }
    }
    closs
  }
}








//   nn.addHiddenLayer(w1, b1, new SigmoidActivationFunction)
//  nn.addOutputLayer(w2, b2, new TanhActivationFunction)




//  require(layers.head.isInstanceOf[HiddenLayer], "Input layer must be of type HiddenLayer (for now.)")
//  require(layers.last.isInstanceOf[OutputLayer], "Output layer must be of type OutputLayer.")
//
//  for (i <- 0 until layers.size - 2) {
//    val slice = layers.view(i, i + 1)
//    println("Slice = " + slice.mkString(", "))
//    require(slice(0).outputSize == slice(1).inputSize, "Layers %d and %d don't agree in output - input sizes".format(i, i+1))
//  }

//      for (layer <- layers.reverse) {
//        layer match {
//          case x: OutputLayer => {
//            louts = x.gradient(louts, verbose=verbose)
//            lastw = x.W
//          }
//          case y: HiddenLayer => y.gradient(louts, lastw, verbose=verbose)
//        }
//      }