package ml.wolfe.term

import org.nd4s.Implicits._
import org.nd4j.linalg.factory.Nd4j

/**
 * @author rockt
 */
object ND4SScratch extends App {
  val arr = (1 to 9).asNDArray(3,3)
  println(arr)

}
