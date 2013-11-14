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
class Index extends Fun[Seq[AnyRef],Int] {

  class ArrayHashing extends HashingStrategy[Array[AnyRef]] {
    def computeHashCode(arg: Array[AnyRef]) = util.Arrays.deepHashCode(arg)
    def equals(o1: Array[AnyRef], o2: Array[AnyRef]) = util.Arrays.deepEquals(o1,o2)
  }

  private val map = new TObjectIntCustomHashMap[Array[AnyRef]](new ArrayHashing)


  def superDomain = new AllOfType[Seq[AnyRef]]//map.keySet().asScala
  def targetSet = Ints
  def apply(v1: Seq[AnyRef]) = index(v1.toArray)
  def isDefinedAt(x: Seq[AnyRef]) = map.containsKey(x.toArray)
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

  //def apply(args:Term[Any]*) = Indexed(this,SeqTerm(args))

  def toVerboseString = {
    val result = new mutable.StringBuilder()
    map.forEachEntry(new TObjectIntProcedure[Array[AnyRef]] {
      def execute(a: Array[AnyRef], b: Int) = {result.append("%40s -> %d\n".format(a.mkString(" , "),b)); true}
    })
    result.toString()
  }
  override def toString = "|Index|=" + map.size()
}
