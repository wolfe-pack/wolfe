package scalapplcodefest

import gnu.trove.strategy.HashingStrategy
import java.util
import gnu.trove.map.custom_hash.TObjectIntCustomHashMap
import scala.collection.mutable
import gnu.trove.procedure.TObjectIntProcedure
import scala.collection.convert.decorateAsScala

/**
 * @author Sebastian Riedel
 */
class Index extends Fun[Seq[Any],Int] {

  class ArrayHashing extends HashingStrategy[Array[AnyRef]] {
    def computeHashCode(arg: Array[AnyRef]) = java.util.Arrays.deepHashCode(arg)
    def equals(o1: Array[AnyRef], o2: Array[AnyRef]) = java.util.Arrays.deepEquals(o1,o2)
  }

  private val map = new TObjectIntCustomHashMap[Array[AnyRef]](new ArrayHashing)

  def funCandidateDom = new AllOfType[Seq[Any]]//map.keySet().asScala
  def funRange = Ints
  def apply(v1: Seq[Any]) = index(v1.map(_.asInstanceOf[AnyRef]).toArray)
  def apply(v1: Term[Any]*) = FunApp(Constant(this),SeqTerm(v1))
  def isDefinedAt(x: Seq[Any]) = true //map.containsKey(x.toArray)
  def index(args:Array[AnyRef]):Int = {
    map.adjustOrPutValue(args,0,map.size)
  }

  def inverse() = {
    val result = new mutable.HashMap[Int,Array[AnyRef]]
    map.forEachEntry(new TObjectIntProcedure[Array[AnyRef]] {
      def execute(a: Array[AnyRef], b: Int) = {result(b) = a; true}
    })
    result
  }

  def vectorToString(vector:Vector, sep:String = "\n") = {
    val inv = inverse()
    val lines = for (i <- vector.activeDomain.toSeq; if vector(i) != 0.0) yield {
      f"${inv(i).mkString(" ")}%20s ${vector(i)}%5.2f"
    }
    lines.mkString(sep)
  }

  def toVerboseString = {
    val result = new mutable.StringBuilder()
    map.forEachEntry(new TObjectIntProcedure[Array[AnyRef]] {
      def execute(a: Array[AnyRef], b: Int) = {result.append("%40s -> %d\n".format(a.mkString(" , "),b)); true}
    })
    result.toString()
  }
  override def toString = "Index"

  def createDenseVector(features:(Seq[Any],Double)*)(dim:Int = features.size) = {
    val vector = new DenseVector(dim)
    for ((feat,value) <- features) {
      val indexOfFeat = index(feat.toArray.asInstanceOf[Array[AnyRef]])
      vector(indexOfFeat) = value
    }
    vector
  }

}

object Index {
  var toDebug:Option[Index] = None
}