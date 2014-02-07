package scalapplcodefest.macros

/**
 * @author Sebastian Riedel
 */
trait WolfeAPI {

  def argmax[T](data:Iterable[T])(where:T => Boolean)(obj:T => Double): T = {
    data.filter(where).maxBy(obj)
  }

  def argmin[T](data:Iterable[T])(where:T => Boolean)(obj:T => Double): T = {
    data.filter(where).minBy(obj)
  }

  def max[T](data:Iterable[T])(where:T => Boolean)(obj:T => Double): Double = {
    obj(argmax(data)(where)(obj))
  }

//
//  def sum[T,N](data:Iterable[T])(obj:T => N)(implicit num:Numeric[N]): N = {
//    data.map(obj).sum(num)
//  }


}


object BruteForceWolfe extends WolfeAPI
