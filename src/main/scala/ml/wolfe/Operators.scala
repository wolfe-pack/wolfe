package ml.wolfe

/**
 * @author Sebastian Riedel
 */
trait Operators {

  def argmax[T](over: Iterable[T])(of: T => Double, where: T => Boolean = (_: T) => true): T = {
    over.filter(where).maxBy(of)
  }

  def argmin[T](over: Iterable[T])(of: T => Double, where: T => Boolean = (_: T) => true): T = {
    over.filter(where).minBy(of)
  }

  def sum[T, N](over: Iterable[T])(of: T => N, where: T => Boolean = (_: T) => true)(implicit n: Numeric[N]): N = {
    over.filter(where).map(of).sum
  }

  def max[T](over: Iterable[T])(of: T => Double, where: T => Boolean = (_: T) => true): Double = {
    over.filter(where).map(of).max
  }



}

object BruteForceOperators extends Operators