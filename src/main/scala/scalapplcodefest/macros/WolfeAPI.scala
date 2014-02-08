package scalapplcodefest.macros

import cc.factorie.WeightsSet
import cc.factorie.optimize.Trainer
import scala.annotation.StaticAnnotation
import scalapplcodefest.MPGraph

/**
 * @author Sebastian Riedel
 */
trait WolfeAPI {

  class MinByDescent(trainer: WeightsSet => Trainer) extends StaticAnnotation
  class MaxByInference(inference: MPGraph => Unit) extends StaticAnnotation

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
