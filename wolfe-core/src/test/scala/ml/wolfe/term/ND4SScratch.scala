package ml.wolfe.term

import org.nd4j.linalg.api.ndarray.INDArray
import org.nd4j.linalg.factory.Nd4j

//import org.nd4s.Implicits._

/**
 * @author rockt
 */
object ND4SScratch extends App {
  val arr = Nd4j.create(Array[Float](1,2,3,4),Array[Int](2,2))
  //val arr = (1 to 9).asNDArray(3,3)
  //println(arr)
  println(arr)
}
