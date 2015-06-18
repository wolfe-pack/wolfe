package ml.wolfe

import com.typesafe.scalalogging.slf4j.LazyLogging
import gnu.trove.strategy.HashingStrategy
import gnu.trove.map.custom_hash.TObjectIntCustomHashMap
import ml.wolfe.term._
import scala.collection.mutable.ArrayBuffer
import scala.collection.{GenMap, mutable}
import gnu.trove.procedure.TObjectIntProcedure
import java.io.{ObjectInputStream, ObjectOutputStream}
import gnu.trove.map.hash.{TIntObjectHashMap, TObjectIntHashMap}
import scalaxy.loops._
import scala.language.postfixOps

// Optional.


/**
 * @author Sebastian Riedel
 */
trait Index extends FeatureIndex {
  def size: Int

  def apply(key: Any): Int = index(key)

  def index(key: Any): Int

  def hasKey(key: Any): Boolean

  def inverse(): GenMap[Int, Any]

  def key(index: Int): Any

  class TemplateIndices(val templateName: Symbol, val domains: IndexedSeq[Dom], val templateIndex: Int) {
    def featureIndex(settings: Array[Setting]): Int = {
      val indices = new ArrayBuffer[Any]
      indices += templateName
      for (i <- 0 until settings.length) indices += domains(i).indexOfSetting(settings(i))
      index(indices)
    }
  }

  private val registeredTemplates = new ArrayBuffer[TemplateIndices]


  def register(templateName: Symbol, domains: Seq[Dom],dense: Boolean) = {
    registeredTemplates.find(_.templateName == templateName) match {
      case Some(i) =>
        require(i.domains == domains)
        i.templateIndex
      case None =>
        val index = registeredTemplates.length
        val newIndices = new TemplateIndices(templateName, domains.toIndexedSeq, index)
        registeredTemplates += newIndices
        index
    }
  }

  def featureIndex(templateIndex: Int, settings: Array[Setting]): Int = {
    registeredTemplates(templateIndex).featureIndex(settings)
  }

  def dom = ???

  def invert(index: Int) = ???

  def keyOf(index: Int) = ???

}

trait FeatureIndex {

  import TermImplicits._

  def register(templateName: Symbol, domains: Seq[Dom], dense: Boolean = false): Int

  def featureIndex(templateIndex: Int, settings: Array[Setting]): Int

  def dom: VectorDom

  def oneHot(name: Symbol, keys: AnyTerm*) =
    Feature(name, keys.toIndexedSeq, Doubles.one, true)(this, this.dom)

  def oneHot(name: Symbol, eager: Boolean, keys: AnyTerm*) =
    Feature(name, keys.toIndexedSeq, Doubles.one, eager)(this, this.dom)

  def oneHot(name: Symbol, value: DoubleTerm, keys: AnyTerm*) =
    Feature(name, keys.toIndexedSeq, value, true)(this, this.dom)

  def oneHot(name: Symbol, eager: Boolean, value: DoubleTerm, keys: AnyTerm*) =
    Feature(name, keys.toIndexedSeq, value, eager)(this, this.dom)

  def keyOf(index: Int): Any

  def toMap(vect: Vect): Map[Any, Double] = {
    vect.activeElements.map { case (i, v) => keyOf(i) -> v }.toMap
  }


}

@SerialVersionUID(100L)
class SimpleIndex extends Serializable with Index {

  private val map = new TObjectIntHashMap[Any]
  private val inversed = new ArrayBuffer[Any]

  def size = map.size()

  def index(args: Any): Int = {
    val oldSize = map.size()
    val current = map.adjustOrPutValue(args, 0, map.size)
    if (map.size > oldSize) inversed += args
    current
  }

  def ++=(that: Map[Any, Int]): Unit = {
    for ((k, v) <- that) {
      map.put(k, v)
    }
  }

  def toMap: Map[Any, Int] = {
    val result = new mutable.HashMap[Any, Int]
    map.forEachEntry(new TObjectIntProcedure[Any] {
      def execute(a: Any, b: Int) = {
        result(a) = b;
        true
      }
    })
    result.toMap

  }

  def key(index: Int) = inversed(index)

  def hasKey(key: Any) = map.containsKey(key)

  def inverse() = {
    val result = new mutable.HashMap[Int, Any]
    map.forEachEntry(new TObjectIntProcedure[Any] {
      def execute(a: Any, b: Int) = {
        result(b) = a
        true
      }
    })
    result
  }

  def toVerboseString = {
    val result = new mutable.StringBuilder()
    map.forEachEntry(new TObjectIntProcedure[Any] {
      def execute(a: Any, b: Int) = {
        result.append("%40s -> %d\n".format(a, b));
        true
      }
    })
    result.toString()
  }

  override def toString = "Index"

  def serialize(stream: ObjectOutputStream) {
    map.writeExternal(stream)
  }

  def deserialize(stream: ObjectInputStream): this.type = {
    val deserializedMap = new TObjectIntCustomHashMap[Any]
    deserializedMap.readExternal(stream)
    this.map.putAll(deserializedMap)
    this
  }

}

class SimpleFeatureIndex(implicit val dom: VectorDom)
  extends FeatureIndex with LazyLogging {
  val dim = dom.dim

  private val sparseMap = new TObjectIntHashMap[IndexedSeq[Any]]
  private var sparseMapUpdated = false
  private val inverseMap = new TIntObjectHashMap[IndexedSeq[Any]]()
  private val registeredTemplates = new ArrayBuffer[TemplateIndices]
  private var currentDenseOffset = 0
  private val index2template = Array.ofDim[Int](dim)




  private def syncInverseMap(): Unit = {
    if (sparseMapUpdated) {
      inverseMap.clear()
      sparseMap.forEachEntry(new TObjectIntProcedure[IndexedSeq[Any]] {
        def execute(a: IndexedSeq[Any], b: Int) = {
          inverseMap.put(b, a)
          true
        }
      })
      sparseMapUpdated = false
    }
  }

  def keyOfSparse(index: Int) = {
    syncInverseMap()
    val settingIndices = inverseMap.get(index).drop(1).asInstanceOf[IndexedSeq[Int]]
    val template = registeredTemplates(index2template(index))
    val values = for ((i,d) <- settingIndices zip template.domains) yield d.indexToValue(i)
    template.templateName +: values

  }

  class TemplateIndices(val templateName: Symbol, val domains: IndexedSeq[Dom],
                        val templateIndex: Int, val sparse: Boolean, val offset: Int) {
    val domainArray = domains.toArray

    private def index(args: IndexedSeq[Any]): Int = {
      val oldSize = sparseMap.size()
      val current = sparseMap.adjustOrPutValue(args, 0, sparseMap.size)
      index2template(current) = templateIndex
      current
    }

    def featureIndexDense(settings: Array[Setting]) = {
      var result = 0
      var i = 0
      while (i < domains.length) {
        val setting = settings(i)
        val dom = domainArray(i)
        val index = dom.indexOfSetting(setting)
        result = index + result * dom.domainSize
        i += 1
      }
      result
    }


    def keyOfDense(index: Int) = {
      val setting = Array.ofDim[Any](domains.length)
      var i = domains.length - 1
      var result = index
      while (i >= 0) {
        val dom = domainArray(i)
        val local = result % dom.domainSize
        val value = dom.indexToValue(local)
        setting(i) = value
        result = result / dom.domainSize
        i -= 1
      }
      (templateName +: setting).toIndexedSeq
    }

    def featureIndex(settings: Array[Setting]): Int = {
      if (sparse) {
        sparseMapUpdated = true
        val indices = new ArrayBuffer[Any]
        indices += templateName
        for (i <- 0 until settings.length optimized) indices += domains(i).indexOfSetting(settings(i))
        dim - index(indices) - 1
      } else {
        featureIndexDense(settings) + offset
      }
    }
  }


  def register(templateName: Symbol, domains: Seq[Dom], dense: Boolean) = {
    registeredTemplates.find(_.templateName == templateName) match {
      case Some(i) =>
        require(i.domains == domains)
        i.templateIndex
      case None =>
        val index = registeredTemplates.length
        val size = domains.map(_.domainSize).product
        //val dense = currentDenseOffset + size < maxDenseCount && size < denseCountThreshold
        val offset = if (!dense) -dim else currentDenseOffset
        if (dense) currentDenseOffset += size
        val newIndices = new TemplateIndices(templateName, domains.toIndexedSeq, index, !dense, offset)
        registeredTemplates += newIndices
        logger.info( s"""Registered $templateName as ${if (dense) "dense" else "sparse"} template""")
        index
    }
  }

  def featureIndex(templateIndex: Int, settings: Array[Setting]) = {
    val template = registeredTemplates(templateIndex)
    template.featureIndex(settings)
  }

  def keyOf(index: Int) = {
    if (index < currentDenseOffset) {
      registeredTemplates.reverse.find(t => !t.sparse && t.offset < index) match {
        case Some(t) =>
          val actual = index - t.offset
          t.keyOfDense(actual)
        case None => sys.error("Value not indexed")
      }
    } else {
      val actual = dim - index - 1
      keyOfSparse(actual)
    }
  }
}


//trait FactorieVectorBuilder {
//  this: Index =>
//
//  private val sparseVectorCache         = new mutable.HashMap[Wolfe.Vector, FactorieVector]()
//  private val oneHotFactorieVectorCache = new mutable.HashMap[(Int, Double), FactorieVector]()
//
//  def toCachedFactorieOneHotVector(component: Any, value: Double) = {
//    val index = this.index(component)
//    val result = oneHotFactorieVectorCache.getOrElseUpdate(index -> value, new SingletonVector(1, index, value))
//    result
//  }
//
//  def toCachedFactorieSparseVector[T](vector: Wolfe.Vector, singletons: Boolean = false): FactorieVector = {
//    val result = sparseVectorCache.getOrElseUpdate(vector, toFreshFactorieSparseVector(vector, singletons))
//    result
//  }
//
//
//  def toFreshFactorieSparseVector[T](vector: Wolfe.Vector, singletons: Boolean = false): FactorieVector = {
//    if (singletons && vector.size == 1) {
//      val singleton = new SingletonVector(1, this.index(vector.head._1), vector.head._2)
//      singleton
//    } else {
//      val sparse = new SparseVector(vector.self.size)
//      for ((key, value) <- vector.self) sparse(this.index(key)) = value
//      sparse
//    }
//  }
//
//  def vectorToString(vector: FactorieVector, sep: String = "\n") = {
//    val inv = inverse()
//    val lines = for (i <- vector.activeDomain.toSeq; if vector(i) != 0.0) yield {
//      f"${ inv(i) }%20s ${ vector(i) }%5.2f"
//    }
//    lines.mkString(sep)
//  }
//}

//class SimpleIndexAndBuilder extends SimpleIndex with FactorieVectorBuilder

object Index {
  var toDebug: Option[Index] = None
}

