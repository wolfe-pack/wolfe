package ml

import org.nd4j.linalg.api.ndarray.INDArray
import org.nd4s.Implicits._

/**
 * @author riedel
 */
package object wolfe {
  type Tensor = INDArray

  def ones(dims: Int*) = Array.fill(dims.product)(1.0).asNDArray(dims: _*)
  def zeros(dims: Int*) = Array.fill(dims.product)(0.0).asNDArray(dims: _*)
}
