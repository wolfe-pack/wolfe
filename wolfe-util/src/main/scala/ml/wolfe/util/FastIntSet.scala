package ml.wolfe.util

/**
 * @author riedel
 */
class FastIntSet(val maxSize: Int) extends scala.collection.mutable.Set[Int] {
  private val indicators = Array.ofDim[Boolean](maxSize)
  private val elements = Array.ofDim[Int](maxSize)
  private var currentSize = 0


  override def clear() = {
    Range(0, currentSize) foreach { i =>
      indicators(elements(i)) = false
    }
    currentSize = 0
  }


  override def size = currentSize

  override def foreach[U](f: (Int) => U) = {
    for (i <- 0 until currentSize) f(elements(i))
  }

  def +=(elem: Int) = {
    if (!indicators(elem)) {
      require(currentSize < maxSize && elem < maxSize)
      elements(currentSize) = elem
      indicators(elem) = true
      currentSize += 1
    }
    this
  }

  def -=(elem: Int) = {
    throw new UnsupportedOperationException("Can't remove elements")
  }

  def contains(elem: Int) = elem < maxSize && indicators(elem)

  def iterator = Range(0, currentSize).iterator map elements
}
