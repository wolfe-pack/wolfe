package ml.wolfe.macros

import ml.wolfe.Wolfe
import Wolfe.Vector
import scala.reflect.macros.Context
import ml.wolfe.macros.OptimizedOperators._
import scala.language.experimental.macros

/**
 * @author Sebastian Riedel
 */
object Losses {

//  def perceptronLoss[T](data: Iterable[T])
//                       (space: Iterable[T], evidence: T => T => Boolean, model: Vector => T => Double)
//                       (weights: Vector): Double = macro perceptronLossImpl[T]
//
//  def perceptronLossImpl[T: c.WeakTypeTag](c: Context)
//                                          (data: c.Expr[Iterable[T]])
//                                          (space: c.Expr[Iterable[T]], evidence: c.Expr[T => T => Boolean], model: c.Expr[Vector => T => Double])
//                                          (weights: c.Expr[Vector]) = {
//    c.universe.reify(sum2(data.splice)(i =>
//      max2(space.splice)(model.splice(weights.splice), evidence.splice(i)) - model.splice(weights.splice)(i)))
//  }

}
