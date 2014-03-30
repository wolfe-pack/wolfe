package ml.wolfe

/**
 * @author Sebastian Riedel
 */
trait Operators {

  import scala.language.implicitConversions

  import Wolfe._

  def argmax[T, N: Ordering](overWhereOf: OverWhereOf[T, N]) = overWhereOf.dom.filter(overWhereOf.filter).maxBy(overWhereOf.obj)
  def argmin[T, N: Ordering](overWhereOf: OverWhereOf[T, N]) = overWhereOf.dom.filter(overWhereOf.filter).minBy(overWhereOf.obj)
  def max[T, N: Ordering](overWhereOf: OverWhereOf[T, N]) = overWhereOf.dom.filter(overWhereOf.filter).map(overWhereOf.obj).max
  def sum[T, N: Numeric](overWhereOf: OverWhereOf[T, N]) = overWhereOf.dom.filter(overWhereOf.filter).map(overWhereOf.obj).sum


}

object BruteForceOperators extends Operators