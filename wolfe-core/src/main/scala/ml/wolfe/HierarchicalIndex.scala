package ml.wolfe

import gnu.trove.map.hash.{TObjectIntHashMap, THashMap}
import scala.collection
import gnu.trove.procedure.{TObjectObjectProcedure, TObjectIntProcedure}

/**
 * @author Sebastian Riedel
 */
class HierarchicalIndex extends Index {

  private var _size = 0

  def size = _size


  def hasKey(key: Any) = ???
  final class NodeIndex(val key: Any) {
    val children = new THashMap[Any, NodeIndex]()
    val indices  = new TObjectIntHashMap[Any]
    private var lastKey:Any = _
    private var lastNode:NodeIndex = _

    def index(key: Any) = {
      val result = indices.adjustOrPutValue(key, 0, _size)
      if (result == _size) _size += 1
      result
    }
    
    def getOrUpdateChild(key:Any) = {
      if (lastKey == key) {
        lastNode
      } else {
        var node = children.get(key)
        if (node == null) {
          node = new NodeIndex(key)
          children.put(key, node)
        }
        lastNode = node
        lastKey = key
        node
      }
    }

    def indexQualified(key:Any):Int = {
      key match {
        case (k1,k2) => getOrUpdateChild(k1).indexQualified(k2)
        case _ => index(key)
      }
    }

    def fillInverse(map: scala.collection.mutable.Map[Int, Any], keyBuilder:Any => Any = identity) {
      indices.forEachEntry(new TObjectIntProcedure[Any] {
        def execute(a: Any, b: Int) = {
          map(b) = keyBuilder(a)
          true
        }
      })
      children.forEachEntry(new TObjectObjectProcedure[Any,NodeIndex] {
        def execute(a: Any, b: NodeIndex) = {
          b.fillInverse(map,key => keyBuilder(a -> key))
          true
        }
      })
    }

  }


  val root = new NodeIndex(null)

  def inverse() = {
    val result = new collection.mutable.HashMap[Int, Any]
    root.fillInverse(result)
    result
  }

  def index(key: Any) = {
    root.indexQualified(key)
  }
}

class HierarchicalIndexAndBuilder extends HierarchicalIndex with FactorieVectorBuilder
