package ml.wolfe.compiler.nd4s

import org.nd4j.linalg.api.ndarray.INDArray
import org.nd4j.linalg.factory.Nd4j
import org.nd4s.Implicits._
import PimpMyND4S._
import org.nd4j.linalg.ops.transforms.Transforms._

/**
 * @author rockt
 */
object ND4SScratch extends App {
  val T = (1 to 8).asNDArray(2,2,2)
  val W = (1 to 6).map(_.toDouble).asNDArray(2,3)
  val x = Seq(1.0, 2.0, 1.5).asNDArray(3)

  println(T.dimsString)
  println(W.dimsString)
  println(x.dimsString)

  //adding constant
  println(x + 2.0)

  //vector outer product
  println(x outer x)

  //sigmoid
  println(sigmoid(x))

  //matrix-vector product
  println(W ** x.t)
}
