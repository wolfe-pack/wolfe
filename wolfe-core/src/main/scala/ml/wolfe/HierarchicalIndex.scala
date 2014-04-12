package ml.wolfe

import gnu.trove.map.hash.{TObjectIntHashMap, THashMap}
import scala.collection
import gnu.trove.procedure.{TObjectObjectProcedure, TObjectIntProcedure}

/**
 * @author Sebastian Riedel
 */
class HierarchicalIndex extends Index {

  private var size = 0

  class NodeIndex(val key: Any) {
    val children = new THashMap[Any, NodeIndex]()
    val indices  = new TObjectIntHashMap[Any]

    def index(key: Any) = {
      val result = indices.adjustOrPutValue(key, 0, size)
      if (result == size) size += 1
      result
    }
    def fillInverse(prefix: Any, map: scala.collection.mutable.Map[Int, Any]) {
      indices.forEachEntry(new TObjectIntProcedure[Any] {
        def execute(a: Any, b: Int) = {
          map(b) = if (prefix == null) a else prefix -> a
          true
        }
      })
      children.forEachEntry(new TObjectObjectProcedure[Any,NodeIndex] {
        def execute(a: Any, b: NodeIndex) = {
          b.fillInverse(if (b.key != null) b.key -> a else a, map)
          true
        }
      })
    }
  }


  val root = new NodeIndex(null)

  def inverse() = {
    val result = new collection.mutable.HashMap[Int, Any]
    root.fillInverse(null, result)
    result
  }

  def index(key: Any) = {
    key match {
      case (k1, k2) =>
        val node = root.children.putIfAbsent(k1, new NodeIndex(k1))
        node.index(k2)
      case _ => root.index(key)
    }
  }
}
