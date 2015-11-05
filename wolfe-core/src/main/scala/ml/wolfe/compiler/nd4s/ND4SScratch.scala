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
  //val arr = Nd4j.create(Array[Float](1,2,3,4),Array[Int](2,2))
  val T = (1 to 8).asNDArray(2,2,2)
  val W = (1 to 6).map(_.toDouble).asNDArray(2,3)
  val x = Seq(1.0, 2.0, 1.5).asNDArray(3)

  println(T.dimsString)
  println(W.dimsString)
  println(x.dimsString)


  println(x + 2.0)

  //vector outer product
  println(x outer x)

  //sigmoid
  println(sigmoid(x))

  //matrix-vector product
  println("???")
  val A = Array[Double](1.0, 2.0, 3.0, 4.0).asNDArray(2,2)
  val B = Array[Double](2.0, 4.0).asNDArray(1,2)
  println(A ** B) //TODO: what is happening here?
  println(B ** A.t)

  println("?")
  val C = Array[Double](1.0, 2.0, 3.0, 4.0).asNDArray(2,2)
  val D = Array[Double](2.0, 4.0).asNDArray(2)

  println(C)
  println(D)
  println(C ** D)

  println(C mul D)
}

object ND4JScratch extends App {
  val nd: INDArray = Nd4j.create(Array[Double](1,2), Array(2)) //row vector
  val nd2: INDArray = Nd4j.create(Array[Double](3,4), Array(2,1)) //column vector
  val nd3: INDArray = Nd4j.create(Array[Double](1,2,3,4), Array(2,2)) //2x2 matrix
  val nd4: INDArray = Nd4j.create(Array[Double](3,4,5,6), Array(2,2)) //2x2 matrix

  println(nd3)
  println(nd2)

  val res = nd3.mmul(nd2)

  println(res)
}

object ND4SBug extends App {
  val W = (1 to 4).asNDArray(2,2)
  val x = (1 to 2).asNDArray(2,1) //column vector
  println("W\n" + W)
  println("x\n" + x)
  println("res\n" + W ** x)
}