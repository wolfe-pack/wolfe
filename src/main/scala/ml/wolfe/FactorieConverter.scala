package ml.wolfe

/**
 * @author Sebastian Riedel
 */
object FactorieConverter {

  import Wolfe.{Vector => WVector}
  import ml.wolfe.{FactorieVector => FVector}

  def toFactorieSparseVector[T](vector: WVector, index: Index): FVector = {
    val sparse = new SparseVector(vector.size)
    for ((key, value) <- vector) sparse(index(Seq(key))) = value
    sparse
  }
  def toWolfeVector(fvector: FVector, index: Index): WVector = {
    val inverse = index.inverse()
    val map = for ((key, value) <- fvector.activeElements; inv <- inverse.get(key)) yield inv(0) -> value
    map.toMap
  }

}