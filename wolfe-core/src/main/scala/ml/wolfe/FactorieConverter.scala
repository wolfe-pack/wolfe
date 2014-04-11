package ml.wolfe

import scala.collection.mutable

/**
 * @author Sebastian Riedel
 */
object FactorieConverter {

  import Wolfe.{Vector => WVector}
  import ml.wolfe.{FactorieVector => FVector}

  //todo: make this thread safe

  def toFreshFactorieSparseVector[T](vector: WVector, index: Index, singletons:Boolean = false): FVector = {
    if (singletons && vector.size == 1) {
      val singleton = new SingletonVector(1,index(vector.head._1),vector.head._2)
      singleton
    } else {
      val sparse = new SparseVector(vector.self.size)
      for ((key, value) <- vector.self) sparse(index(key)) = value
      sparse
    }
  }

  def toFactorieDenseVector[T](vector: WVector, index: Index): FVector = {
    val dense = new DenseVector(vector.self.size + 1000)
    for ((key, value) <- vector.self) dense(index(key)) = value
    dense
  }

  def toWolfeVector(fvector: FVector, index: Index): WVector = {
    val inverse = index.inverse()
    val map = for ((key, value) <- fvector.activeElements; inv <- inverse.get(key)) yield inv -> value
    new Wolfe.Vector(map.toMap)
  }

}