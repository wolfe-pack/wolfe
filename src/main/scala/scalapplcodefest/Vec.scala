package scalapplcodefest

import gnu.trove.map.hash.TIntDoubleHashMap
import gnu.trove.procedure.TIntDoubleProcedure
import scalaxy.loops._


/**
 * An interface to vectors.
 *
 * @author Sebastian Riedel
 */
trait Vec {

  def apply(index:Int):Double

  def +(that:Vec):Vec = Vec.sum(Array(this,that))

  def activeIndices:Traversable[Int]

  def dot(that:Vec):Double

  override def toString = {
    activeIndices.map(i => "%5d %f".format(i,this(i))).mkString("\n")
  }

  def toMap = activeIndices.map(i => i -> apply(i)).toMap

}

object Vec {

  def sum(args:Array[Vec]) = {
    val result = new SparseTroveVec(args.length)
    for (arg <- args) result += arg
    result
  }

  def sumSingletons(args:Array[UnitVector]) = {
    val result = new SparseTroveVec(args.length)
    for (arg <- args) result += arg
    result
  }

  val zero = new Vec {
    def dot(that: Vec) = 0.0
    def apply(index: Int) = 0.0
    def activeIndices = Seq.empty
    override def toString = "VEC-ZERO"
  }
}

final class UnitVector(val index:Int, val value:Double = 1.0) extends Vec {
  def apply(index: Int) = if (this.index == index) value else 0.0

  def ===(that:UnitVector) = this.index == that.index && this.value == that.value
  def activeIndices = Seq(index)
  def dot(that: Vec) = that(index) * value
  override def equals(p1: Any) = p1 match {
    case x:UnitVector => x.index == index && x.value == value
    case _ => false
  }
}

final class SmallSparseVec(val indices:Array[Int],val values:Array[Double])(val size:Int=values.length) extends Vec {
  def apply(index: Int):Double = {
    val current = 0
    while (current <= size) {
      if (indices(current) == index) return values(current)
    }
    0.0
  }
  def activeIndices = indices
  def dot(that: Vec) = {
    var result = 0.0
    for (i <- (0 until size).optimized)
      result += values(i) * that(i)
    result
  }
}

final class SparseTroveVec(initialCapacity:Int) extends Vec {
  val map = new TIntDoubleHashMap(initialCapacity)

  def update(index:Int, value:Double) {
    map.put(index,value)
  }

  def +=(that:Vec):this.type = {
    for (i <- that.activeIndices) this(i) = this(i) + that(i)
    this
  }

  def +=(that:UnitVector):this.type =  {
    map.adjustOrPutValue(that.index,that.value,that.value)
    this
  }

  def apply(index: Int) = map.get(index)
  def activeIndices = map.keys()
  def dot(that: Vec) = {
    var result = 0.0
    map.forEachEntry(new TIntDoubleProcedure {
      def execute(index: Int, value: Double) = {
        result += that(index) * value
        true
      }
    })
    result
  }

  override def equals(p1: Any) = p1 match {
    case x:SparseTroveVec => x.map.equals(map)
    case _ => false
  }

}

final class DenseVec(initialCapacity:Int, multiplier:Double = 2.0) extends Vec {
  private var values = new Array[Double](initialCapacity)
  def apply(index: Int) = if (index < values.length) values(index) else 0.0
  def activeIndices = values.indices
  def dot(that: Vec) = {
    var result = 0.0
    for (index <- that.activeIndices) result += values(index) * that(index)
    result
  }
  def +=(that:Vec):this.type = add(that,1.0)

  def update(index:Int,value:Double) {
    values(index) = value
  }

  def add(that:Vec, scale:Double=1.0):this.type = {
    for (i <- that.activeIndices) {
      ensureCapacity(i)
      values(i) += that(i) * scale
    }
    this
  }

  def ensureCapacity(i: Int) {
    if (i >= values.length) {
      val old = values
      values = new Array[Double](math.round(i * multiplier).toInt)
      System.arraycopy(old, 0, values, 0, old.length)
    }
  }
}