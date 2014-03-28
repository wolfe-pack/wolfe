package ml.wolfe

/**
 * @author Sebastian Riedel
 */
trait Operators {

  def argmax[T](data: Iterable[T])(obj: T => Double, where: T => Boolean = (_:T) => true): T = {
    data.filter(where).maxBy(obj)
  }
}

object BruteForceOperators extends Operators