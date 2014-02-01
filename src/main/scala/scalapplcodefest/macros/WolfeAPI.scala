package scalapplcodefest.macros

/**
 * @author Sebastian Riedel
 */
trait WolfeAPI {

  def argmax[T,N](data:Iterable[T])(where:T => Boolean)(obj:T => N)(implicit num:Ordering[N]): T = {
    data.filter(where).maxBy(obj)(num)
  }

}


object BruteForceWolfe extends WolfeAPI
