package scalapplcodefest.macros

/**
 * @author Sebastian Riedel
 */
trait WolfeAPI {

  def argmax[T](data:Iterable[T])(where:T => Boolean)(obj:T => Double): T = {
    data.filter(where).maxBy(obj)
  }

}


object BruteForceWolfe extends WolfeAPI
