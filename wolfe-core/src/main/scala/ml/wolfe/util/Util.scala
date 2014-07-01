package ml.wolfe.util

import java.io.{FileInputStream, InputStream}
import java.util
import scala.annotation.tailrec
import scala.io.Source
import scala.collection.mutable
import java.util.concurrent.TimeUnit

import scala.reflect.ClassTag

/**
 * @author Sebastian Riedel
 */
object Util {

  /**
   * Loads a resource as stream. This returns either a resource in the classpath,
   * or in case no such named resource exists, from the file system.
   */
  def getStreamFromClassPathOrFile(name: String): InputStream = {
    val is: InputStream = getClass.getClassLoader.getResourceAsStream(name)
    if (is == null) {
      new FileInputStream(name)
    }
    else {
      is
    }
  }

  //todo: can use arrays of length factors.length
  def cartesianProduct[S](factors: Seq[Seq[S]]) : Seq[Seq[S]] = {
    @tailrec
    def productWithSuffixes(suffixes:Seq[List[S]], remainingFactors:Seq[Seq[S]]) : Seq[List[S]] =
      remainingFactors.headOption match {
        case None => suffixes
        case Some(head) => {
          def newSuffixes = for (v <- head; s <- suffixes) yield v +: s
          productWithSuffixes(newSuffixes, remainingFactors.tail)
        }
      }
    productWithSuffixes(Seq(List()), factors.reverse)
  }
}

object LabelledTensor {
  def onExistingArray[L, T: ClassTag](labels:Array[L], dimensions: L => Int, array: Array[T] ) =
    new LabelledTensor(labels, dimensions, array)

  def onNewArray[L, T: ClassTag](labels:Array[L], dimensions: L => Int, default:T) =
    new LabelledTensor(labels, dimensions, Array.fill(labels.map(dimensions).product)(default))

  class LabelledTensor[L, T: ClassTag](val labels:Array[L], val dimensions: L => Int, val array: Array[T] ) {

    if(array.length != labels.map(dimensions).product) sys.error("LabelledTensor array is not the right size")
    private val indexSteps: L => Int = labels.zip(labels.scanRight(1)((l, acc) => dimensions(l) * acc).tail).toMap

    def allMuls(forLabels:Seq[L]) = Util.cartesianProduct(
      forLabels.map(l => {(0 until dimensions(l)).map(l -> _)})
    ).map(_.toMap)

    def fill(f:Map[L, Int] => T) = for((mul, idx) <- allMuls(labels).zipWithIndex) array(idx) = f(mul)

    private def mulToIndex(mul: Map[L, Int]) : Int = (
      for( (label, idx) <- mul ) yield indexSteps(label) * idx
    ).sum

    def fold[S: ClassTag](keepLabels: Array[L], z: S, op: (S, T) => S, destination:Array[S]) : LabelledTensor[L, S] = {
      val destIndicesBase = allMuls(keepLabels).map(mulToIndex)
      val foldedIndicesBase = allMuls(labels diff keepLabels).map(mulToIndex)
      for((i, j) <- destIndicesBase.zipWithIndex) {
        val toFold = for(k <- foldedIndicesBase) yield array(i + k)
        destination(j) = toFold.foldLeft(z)(op)
      }
      LabelledTensor.onExistingArray[L, S](keepLabels, dimensions, destination)
    }

    def fold[S: ClassTag](keepDims: Array[L], z: S, op: (S, T) => S) : LabelledTensor[L, S] = {
      val destination  = Array.fill[S](keepDims.map(dimensions(_)).product)(z)
      fold(keepDims, z, op, destination)
    }

    def elementWiseOp[U, V:ClassTag](that: LabelledTensor[L, U], op: (T, U) => V, destination:Array[V]) : LabelledTensor[L, V] = {
      val sameLabels = this.labels intersect that.labels
      val sameMulsBase = allMuls(sameLabels)
      val sameIndicesBase = sameMulsBase.map(mulToIndex)
      val diffIndicesBase = allMuls(labels diff sameLabels).map(mulToIndex)
      for((i,j) <- sameIndicesBase.zipWithIndex) {
        val x = that.array(j)
        for(k <- diffIndicesBase) {
          destination(i + k) = op(this.array(i+k), x)
        }
      }
      LabelledTensor.onExistingArray[L,V](labels, dimensions, destination)
    }

    def elementWiseOp[U](that: LabelledTensor[L, U], op: (T, U) => T) : LabelledTensor[L, T] =
      elementWiseOp(that, op, this.array)
  }
}
/*object LabelledTensor {

  /*def apply[I, T: ClassTag](labels:Array[I], dimensions: Array[Int], array:Array[T]) =
    new LabelledTensor(labels, dimensions, array)*/
  
  def apply[I, T: ClassTag](labels:Array[I], dimensions: Array[Int], default:T) =
    new LabelledTensor(labels, dimensions, Array.fill[T](dimensions.product)(default))

  def onArray[I, T: ClassTag](labels:Array[I], dimensions: Array[Int], array:Array[T]) =
    new LabelledTensor(labels, dimensions, array)

  class LabelledTensor[I, T: ClassTag](val labels:Array[I], val dimensions: Array[Int], val array: Array[T] ) {
    import scalaxy.loops._

    if(array.length != dimensions.product) sys.error("LabelledTensor array is not the right size")

    private val indexSteps: Array[Int] = dimensions.scanRight(1)(_ * _).tail.toArray

    private def multiToIndex(multi: Seq[Int]): Int = {
      var idx = 0
      for ((n, i) <- multi.zipWithIndex) {
        idx = idx + n * indexSteps(i)
      }
      idx
    }
    private def indexToMulti(index: Int): Array[Int] = {
      val mul = Array.ofDim[Int](dimensions.length)
      for (i <- (0 until dimensions.length).optimized) {
        mul(i) = (index / indexSteps(i)) % dimensions(i)
      }
      mul
    }
    private def indexToMultiFiltered(index: Int, dimIdxs: Array[Int]): Array[Int] = {
      val mul = Array.ofDim[Int](dimIdxs.length)
      for (i <- (0 until dimIdxs.length).optimized) {
        mul(i) = (index / indexSteps(dimIdxs(i))) % dimensions(dimIdxs(i))
      }
      mul
    }

    def apply(idxs: Seq[Int]): T = array(multiToIndex(idxs))
    def update(idxs: Seq[Int], elem: T): Unit = array(multiToIndex(idxs)) = elem

    //def apply(idxs: Seq[I]): T = apply(idxs.map(labels.indexOf(_)))
    //def update(idxs: Seq[I], elem: T): Unit = update(idxs.map(labels.indexOf(_)), elem)

    def fold[S: ClassTag](keepDims: Array[I], z: S, op: (S, T) => S, destination:Array[S]) : LabelledTensor[I, S] = {
      for(i <- (0 until destination.length).optimized) destination(i)=z
      val keepIndices = keepDims.map(labels.indexOf(_))
      val reduced = LabelledTensor.onArray[I, S](keepDims, keepIndices.map(dimensions(_)), destination)
      for (i <- (0 until array.length).optimized) {
        val mul = indexToMultiFiltered(i, keepIndices)
        reduced(mul) = op(reduced(mul), array(i))
      }
      reduced
    }

    def fold[S: ClassTag](keepDims: Array[I], z: S, op: (S, T) => S) : LabelledTensor[I, S] = {
      val keepIndices = keepDims.map(labels.indexOf(_))
      val destination  = Array.fill[S](keepIndices.map(dimensions(_)).product)(z)
      fold(keepDims, z, op, destination)
    }

    def elementWise[U, V](that: LabelledTensor[I, U], op: (T, U) => V, destination:Array[V]) : Array[V] = {
      /*val dimIdxs = that.labels.map(labels.indexOf(_))
      for (i <- (0 until array.length).optimized) {
        destination(i) = op(array(i), that(indexToMultiFiltered(i, dimIdxs)))
      }
      destination*/
      val thatIndexSteps = that.labels.map(x => indexSteps(this.labels.indexOf(x)))
      val distinctLabels = this.labels diff that.labels
      val distinctIndexSteps = distinctLabels.map(x => indexSteps(labels.indexOf(x)))
      val initialMultis  = Util.cartesianProduct(distinctLabels.map(x =>
        0 until dimensions(labels.indexOf(x))))
      val initialIndices = initialMultis.map(mul => {
        var x = 0
        for(i <- (0 until mul.length).optimized) {
          x = x + mul(i)*distinctIndexSteps(i)
        }
        x
      })
      for(i <- (0 until that.array.length).optimized) {
        val thatMul = that.indexToMulti(i)
        val firstIndex = {
          var x = 0
          for(j <- (0 until thatMul.length).optimized) x = x + thatMul(j) * thatIndexSteps(j)
          x
        }
        for(j <- (0 until initialIndices.length).optimized) {
          val r = firstIndex + j
          destination(r) = op(array(r), that.array(i))
        }
      }
      destination
    }

    def elementWise(that: LabelledTensor[I, T], op: (T, T) => T) : Array[T] = elementWise(that, op, array)


    override def toString = array.mkString(",")
  }
}*/



/**
 * Code for IRIS dataset.
 */
object Iris {

  implicit val classes = Seq(Label("Iris-setosa"), Label("Iris-versicolor"), Label("Iris-virginica"))

  case class Label(label: String)
  case class IrisData(sepalLength: Double, sepalWidth: Double, petalLength: Double, petalWidth: Double, irisClass: Label)

  def loadIris() = {
    val stream = Util.getStreamFromClassPathOrFile("ml/wolfe/datasets/iris/iris.data")
    val data = for (line <- Source.fromInputStream(stream).getLines().toBuffer if line.trim != "") yield {
      val Array(sl, sw, pl, pw, ic) = line.split(",")
      IrisData(sl.toDouble, sw.toDouble, pl.toDouble, pw.toDouble, Label(ic))
    }
    stream.close()
    data
  }
}

object Timer {
  val timings = new mutable.HashMap[String,Long]()

  def time[A](name:String)(f: => A) = {
    val start = System.nanoTime
    val result = f
    val time: Long = TimeUnit.MILLISECONDS.convert(System.nanoTime - start, TimeUnit.NANOSECONDS)
    timings(name) = time
    result
  }

  def reported(name:String):Long = timings.getOrElse(name, -1)

  def getTimeString(seconds: Int): String = {
    def buildTimeString(seconds: Int, acc: String): String = {
      if (seconds < 60) acc + "%ds".format(seconds)
      else if (seconds < 3600) acc + buildTimeString(seconds % 60, "%dm ".format(seconds / 60))
      else if (seconds < 86400) acc + buildTimeString(seconds % 3600, "%dh ".format(seconds / 3600))
      else if (seconds < 604800) acc + buildTimeString(seconds % 86400, "%dd ".format(seconds / 86400))
      else if (seconds < 4233600) acc + buildTimeString(seconds % 604800, "%dw ".format(seconds / 604800))
      else "very long"
    }
    buildTimeString(seconds, "")
  }

  def getTimeString(milliseconds: Long): String = getTimeString((milliseconds / 1000).toInt)
}


/**
 * A function that turns "lifted" functions to Options into partial functions such that repeated calls
 * in isDefinedAt and apply are avoided by caching results.
 * @param f the lifted function to turn into a partial function.
 */
case class CachedPartialFunction[A,B](f:A => Option[B]) extends PartialFunction[A,B] {
  private var cacheArg:A = _
  private var cacheResult:Option[B] = None

  def cache(x:A) = {
    if (x != cacheArg) {
      cacheArg = x
      cacheResult = f(cacheArg)
    }
  }

  def isDefinedAt(x: A) = {
    cache(x)
    cacheResult.isDefined
  }

  def apply(x:A) = {
    cache(x)
    cacheResult.get
  }

}
