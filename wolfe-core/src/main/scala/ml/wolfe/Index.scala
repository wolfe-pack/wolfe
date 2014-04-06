package ml.wolfe

import gnu.trove.strategy.HashingStrategy
import gnu.trove.map.custom_hash.TObjectIntCustomHashMap
import scala.collection.mutable
import gnu.trove.procedure.TObjectIntProcedure
import java.io.{ObjectInputStream, ObjectOutputStream}

/**
 * @author Sebastian Riedel
 */
@SerialVersionUID(100L)
class Index extends Serializable {

  class ArrayHashing extends HashingStrategy[Array[AnyRef]] {
    def computeHashCode(arg: Array[AnyRef]) = java.util.Arrays.deepHashCode(arg)
    def equals(o1: Array[AnyRef], o2: Array[AnyRef]) = java.util.Arrays.deepEquals(o1, o2)
  }

  private var map = new TObjectIntCustomHashMap[Array[AnyRef]](new ArrayHashing)

  //map.keySet().asScala
  def apply(v1: Seq[Any]) = index(v1.map(_.asInstanceOf[AnyRef]).toArray)
  def size = map.size()
  def isDefinedAt(x: Seq[Any]) = true
  //map.containsKey(x.toArray)
  def index(args: Array[AnyRef]): Int = {
    map.adjustOrPutValue(args, 0, map.size)
  }

  def inverse() = {
    val result = new mutable.HashMap[Int, Array[AnyRef]]
    map.forEachEntry(new TObjectIntProcedure[Array[AnyRef]] {
      def execute(a: Array[AnyRef], b: Int) = { result(b) = a; true }
    })
    result
  }

  def vectorToString(vector: FactorieVector, sep: String = "\n") = {
    val inv = inverse()
    val lines = for (i <- vector.activeDomain.toSeq; if vector(i) != 0.0) yield {
      f"${ inv(i).mkString(" ") }%20s ${ vector(i) }%5.2f"
    }
    lines.mkString(sep)
  }

  def toVerboseString = {
    val result = new mutable.StringBuilder()
    map.forEachEntry(new TObjectIntProcedure[Array[AnyRef]] {
      def execute(a: Array[AnyRef], b: Int) = { result.append("%40s -> %d\n".format(a.mkString(" , "), b)); true }
    })
    result.toString()
  }
  override def toString = "Index"

  def createDenseVector(features: (Seq[Any], Double)*)(dim: Int = features.size) = {
    val vector = new DenseVector(dim)
    for ((feat, value) <- features) {
      val indexOfFeat = index(feat.toArray.asInstanceOf[Array[AnyRef]])
      vector(indexOfFeat) = value
    }
    vector
  }

  def serialize(stream: ObjectOutputStream) {
    map.writeExternal(stream)
  }

  def deserialize(stream: ObjectInputStream): this.type = {
    val deserializedMap = new TObjectIntCustomHashMap[Array[AnyRef]](new ArrayHashing)
    deserializedMap.readExternal(stream)
    this.map.putAll(deserializedMap)
    this
  }

  private val sparseVectorCache = new mutable.HashMap[Wolfe.Vector, FactorieVector]()

  private val oneHotFactorieVectorCache = new mutable.HashMap[(Int, Double), FactorieVector]()

  def toCachedFactorieOneHotVector(component: Any, value: Double) = {
    val index = this(Seq(component))
    val result = oneHotFactorieVectorCache.getOrElseUpdate(index -> value, new SingletonVector(1, index, value))
    result
  }

  def toCachedFactorieSparseVector[T](vector: Wolfe.Vector, singletons: Boolean = false): FactorieVector = {
    val result = sparseVectorCache.getOrElseUpdate(vector, toFreshFactorieSparseVector(vector, singletons))
    result
  }


  def toFreshFactorieSparseVector[T](vector: Wolfe.Vector, singletons: Boolean = false): FactorieVector = {
    if (singletons && vector.size == 1) {
      val singleton = new SingletonVector(1, this(Seq(vector.head._1)), vector.head._2)
      singleton
    } else {
      val sparse = new SparseVector(vector.self.size)
      for ((key, value) <- vector.self) sparse(this(Seq(key))) = value
      sparse
    }
  }


}

object Index {
  var toDebug: Option[Index] = None
}