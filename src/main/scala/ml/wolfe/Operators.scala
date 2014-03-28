package ml.wolfe

/**
 * @author Sebastian Riedel
 */
trait Operators {

  import scala.language.implicitConversions

  import Wolfe._

  def argmax2[T](over: Iterable[T])(of: T => Double, where: T => Boolean = (_: T) => true): T = {
    over.filter(where).maxBy(of)
  }

  def argmin[T](over: Iterable[T])(of: T => Double, where: T => Boolean = (_: T) => true): T = {
    over.filter(where).minBy(of)
  }

  def sum2[T, N](over: Iterable[T])(of: T => N, where: T => Boolean = (_: T) => true)(implicit n: Numeric[N]): N = {
    over.filter(where).map(of).sum
  }

  def max2[T](over: Iterable[T])(of: T => Double, where: T => Boolean = (_: T) => true): Double = {
    over.filter(where).map(of).max
  }

  //  def argmax[T](overWhereOf: OverWhereOf[T]) = overWhereOf.dom.filter(overWhereOf.filter).maxBy(overWhereOf.obj)
  def argmax[T, N: Ordering](overWhereOf: OverWhereOf[T, N]) = overWhereOf.dom.filter(overWhereOf.filter).maxBy(overWhereOf.obj)
  def max[T, N: Ordering](overWhereOf: OverWhereOf[T, N]) = overWhereOf.dom.filter(overWhereOf.filter).map(overWhereOf.obj).max
  def sum[T, N: Numeric](overWhereOf: OverWhereOf[T, N]) = overWhereOf.dom.filter(overWhereOf.filter).map(overWhereOf.obj).sum

  def argmax3[T](over: Iterable[T])(whereOf: OverWhereOf[T,Double]) = over.filter(whereOf.filter).maxBy(whereOf.obj)

  case class Where[T](dom:Iterable[T], filter:T=>Boolean) {
    def apply(obj:T => Double) = dom.filter(filter).maxBy(obj)
    def where(where:T=>Boolean) = copy(filter = where)
  }
  def argmax4[T](over:Iterable[T]) = Where(over, (t:T) => true)


}

object BruteForceOperators extends Operators