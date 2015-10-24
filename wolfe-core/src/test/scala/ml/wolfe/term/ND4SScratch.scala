package ml.wolfe.term

import org.nd4j.linalg.api.ndarray.INDArray
//import org.nd4s.Implicits._
import org.nd4j.linalg.factory.Nd4j

/**
 * @author rockt
 */
object ND4SScratch extends App {
  val arr = (1 to 9).asNDArray(3,3)
  println(arr)

  //val arr1 = Nd4j.create(Array(1.0))
  //println(arr1)
}
