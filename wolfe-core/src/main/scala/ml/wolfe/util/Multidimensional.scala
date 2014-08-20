package ml.wolfe.util

import scala.annotation.tailrec
import scala.reflect.ClassTag
import scalaxy.loops._

/**
 * @author luke
 */
object Multidimensional { //todo: views!

  /**
   * Returns the cartesian product of some sequences in lexicographical order
   * i.e. (000, 001, ..., 010, ...)
   * @param factors The sequences to take the product of
   * @tparam S the type of the elements
   * @return The cartesian product
   */
  def cartesianProduct[S](factors: Seq[Seq[S]]): Seq[Seq[S]] = {
    @tailrec
    def productWithSuffixes(suffixes: Seq[List[S]], remainingFactors: Seq[Seq[S]]): Seq[List[S]] =
      remainingFactors.headOption match {
        case None => suffixes
        case Some(head) =>
          def newSuffixes = for (v <- head; s <- suffixes) yield v +: s
          productWithSuffixes(newSuffixes, remainingFactors.tail)
      }
    productWithSuffixes(Seq(List()), factors.reverse)
  }


  object LabelledTensor {
    /**
     * Create a LabelledTensor as a wrapper around an existing array.
     * @param array The array to wrap the LabelledTensor object around
     */
    def onExistingArray[L, T: ClassTag](labels: Array[L], dimensions: L => Int, array: Array[T]) =
      new LabelledTensor(labels, dimensions, array)

    /**
     * Create a LabelledTensor object on a new array
     * @param default the default value to populate the tensor
     */
    def onNewArray[L, T: ClassTag](labels: Array[L], dimensions: L => Int, default:T = null.asInstanceOf[T]) =
      new LabelledTensor(labels, dimensions, Array.fill[T](labels.map(dimensions).product)(default))
  }


  case class LabelledTensorDimensionError(msg:String = "") extends Exception(msg)

  /**
   * Wrapper for an Array so that it can be used as a tensor, with labelled dimensions
   * @param labels The labels for each dimension of the tensor
   * @param dimensions Maps labels to the size of the dimension they represent
   * @param array The array wrapped by the LabelledTensor Object
   * @tparam L The type of the dimension labels
   * @tparam T The type of the elements
   */
  class LabelledTensor[L, T: ClassTag](val labels: Array[L], val dimensions: L => Int, val array: Array[T]) {
    val size = (labels map dimensions).product
    type MultiIndex = Seq[(L, Int)]

    if (array.length != labels.map(dimensions).product) {
      throw new LabelledTensorDimensionError()
    }
    private val indexSteps: L => Int = labels.zip(labels.scanRight(1)((l, acc) => dimensions(l) * acc).tail).toMap

    private def allMuls(forLabels: Seq[L] = labels) : Seq[MultiIndex] = cartesianProduct(
      forLabels.map(l => { (0 until dimensions(l)).map(l -> _) })
    )

    private def assertEquivalent(arr1:Array[L], arr2:Array[L]) =
      if(arr1.length != arr2.length || (arr2 diff arr1).nonEmpty)
        throw new LabelledTensorDimensionError("Two LabelledTensors should live in the same space, but did not.")

    private def assertContains(arr1:Array[L], arr2:Array[L]) =
      if((arr2 diff arr1).nonEmpty)
        throw new LabelledTensorDimensionError("LabelledTensor error. One Labelled tensor should live in a subspace of another, but did not.")


    // ------------------------------------------------------

    // Useful for testing, but I'm not really that keen on these being public   -Luke
    private def mulToIndex(mul: MultiIndex): Int = (
                                           for ((label, idx) <- mul) yield indexSteps(label) * idx
                                           ).sum

    private def indexToMul(i:Int) : MultiIndex =
      for (l <- labels) yield l -> (i / indexSteps(l)) % dimensions(l)

    // --------------------- Interface ----------------------

    def apply(mul: MultiIndex) : T = array(mulToIndex(mul))
    def update(mul: MultiIndex, value:T) : Unit = array(mulToIndex(mul)) = value
    def apply(i:Int) : T = array(i)
    def update(i:Int, value:T) : Unit = array(i) = value

    /** fill all elements in this with some function of their [[MultiIndex]] */
    def fillBy(f: MultiIndex => T) = for ((mul, idx) <- allMuls(labels).zipWithIndex) array(idx) = f(mul)

    /** fill all elements in this with a constant */
    def fill(x: T) = fillBy(mul => x)

    def zipWithIndex = array.view zip allMuls()

    /**
     * Fold this into a smaller LabelledTensor using a binary operation. Dimensions which appear in '''this'''
     * but not '''destination''' are folded over (e.g. to computute max or sum marginals)
     * @param z The fold start value
     * @param op The binary operation
     * @param destination The destination
     * @tparam S The type of the elements in the result
     * @return the destination LabelledTensor
     */
    def foldInto[S: ClassTag](z: S, op: (S, T) => S, destination: LabelledTensor[L, S]): LabelledTensor[L, S] = {
      assertContains(labels, destination.labels)
      val keepLabels = destination.labels
      val keepIndicesThis = allMuls(keepLabels).map(mulToIndex)
      val foldIndicesThis = allMuls(labels diff keepLabels).map(mulToIndex)
      for ((iThis, iDest) <- keepIndicesThis.zipWithIndex) {
        val toFold = for (jThis <- foldIndicesThis) yield array(iThis + jThis)
        destination.array(iDest) = toFold.foldLeft(z)(op)
      }
      destination
    }

    /** Fold this LabelledTensor, creating a new LabelledTensor to hold the result. @see [[fold(z, op, dest)]]
      * @param keepLabels the dimension labels to keep (i.e the labels of the result)
      */
    def fold[S: ClassTag](keepLabels: Array[L], z: S, op: (S, T) => S): LabelledTensor[L, S] = {
      val destination = LabelledTensor.onNewArray[L, S](keepLabels, dimensions)
      foldInto(z, op, destination)
    }



    /**
     * Create a new LabelledTensor object, with the same labels a different order for array indexing
     * @param order The order of dimensions in the new array
     * @param allowSameArray Should the result use the same array object if the order is the same?
     * @return The new LabelledTensor object
     */
    def permute(order: Array[L], allowSameArray: Boolean = false): LabelledTensor[L, T] = {
      assertEquivalent(order, labels)
      val destination = Array.ofDim[T](order.map(dimensions).product)
      val destIndices = allMuls(order).map(mulToIndex)
      for ((i, j) <- destIndices.zipWithIndex) {
        destination(j) = array(i)
      }
      LabelledTensor.onExistingArray[L, T](order, dimensions, destination)
    }



    /**
     * Create a new LabelledTensor from this and another, using an elementwise binary operation.
     * @note The result has the same size as '''this'''. '''that''' may be smaller, in which case each
     *       element of '''that''' will be used to generate many corresponding elements in '''destination'''
     * @param that LabelledTensor whose elements are the second argument of op
     * @param op the binary operation
     * @param destination the LabelledTensor to store the result
     * @tparam U The type of the elements in that
     * @tparam V The type of the elements in destination
     * @return destination
     */
    def elementWiseToDestination[U, V: ClassTag]
    (that: LabelledTensor[L, U], op: (T, U) => V, destination: LabelledTensor[L, V]): LabelledTensor[L, V] = {
      assertEquivalent(labels, destination.labels)
      assertContains(labels, that.labels)
      if (this.labels.toSeq == destination.labels.toSeq) {
        if(this.labels.toSeq == that.labels.toSeq) {
          //All arrays are of the same dimension, and indexed the same way
          for (i <- (0 until destination.array.length).optimized)
            destination.array(i) = op(this.array(i), that.array(i))
        } else {
          if(this.labels.length == that.labels.length) LoggerUtil.once(LoggerUtil.warn,
            "LabelledTensor elementwise this and that have permuted labels. This is inefficient.",
            labels.mkString("(",",",")") + " != " + that.labels.mkString("(",",",")"))

          val sameIndices = that.allMuls() map mulToIndex
          val extraIndices = allMuls(labels diff that.labels) map mulToIndex
          for ((iThis, iThat) <- sameIndices.zipWithIndex) {
            val x = that.array(iThat)
            for (jThis <- extraIndices) destination.array(iThis + jThis) = op(this.array(iThis + jThis), x)
          }
        }

      } else {
        LoggerUtil.once(LoggerUtil.warn,
          "LabelledTensor elementwise this and destination have permuted labels. This is inefficient",
          labels.mkString("(",",",")") + " != " + destination.labels.mkString("(",",",")"))

        val sameIndices = that.allMuls() map (m => (mulToIndex(m), destination.mulToIndex(m)))
        val extraIndices = allMuls(labels diff that.labels) map (m => (mulToIndex(m), destination.mulToIndex(m)))
        for (((iThis, iDest), iThat) <- sameIndices.zipWithIndex) {
          val x = that.array(iThat)
          for ((jThis, jDest) <- extraIndices) destination.array(iDest + jDest) = op(this.array(iThis + jThis), x)
        }
      }

      destination
    }

    /** Elementwise operation, storing the result in a new LabelledTensor.
      * @see [[elementWiseToDestination]] */
    def elementWiseOp[U, V:ClassTag](that: LabelledTensor[L, U], op: (T, U) => V): LabelledTensor[L, V] =
      elementWiseToDestination[U, V](that, op, LabelledTensor.onNewArray[L, V](labels, dimensions))

    /** Elementwise operation, storing the result in this LabelledTensor.
      * @see [[elementWiseToDestination]] */
    def elementWiseOpInPlace[U](that: LabelledTensor[L, U], op: (T, U) => T): LabelledTensor[L, T] =
      elementWiseToDestination(that, op, this)

    /** Elementwise addition */
    def +=(that: LabelledTensor[L, T])(implicit n : Numeric[T]) = elementWiseOpInPlace(that, n.plus)

    /** Index of maximum  */
    def maxIndex(implicit o : Ordering[T]) = indexToMul((0 until array.length).maxBy(array)(o))

    /** Copy all elements from another labelled tensor to this one.
      * @note If '''source''' has fewer dimensions, then elements will be repeated.
      */
    def copyFrom(source: LabelledTensor[L, T]): LabelledTensor[L, T] =
      elementWiseOpInPlace[T](source, (_, x) => x)

    def map[U:ClassTag](f:T => U) = LabelledTensor.onExistingArray[L, U](labels, dimensions, array.map(f))

    override def clone() = LabelledTensor.onExistingArray(labels, dimensions, array.clone())

    override def toString = {
      val d = labels.reverse.find(indexSteps(_) >= math.sqrt(array.length))
      d match {
        case None => array.map(_.toString.take(4)).mkString("(", ",", ")")
        case Some(x) => array.map(_.toString.take(4)).grouped(indexSteps(x)).map(_.mkString("(", ",", ")")).mkString("\n")
      }
    }
  }

}