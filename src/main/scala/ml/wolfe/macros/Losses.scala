package ml.wolfe.macros

import ml.wolfe.Wolfe
import Wolfe.Vector
import scala.reflect.macros.Context
import ml.wolfe.macros.OptimizedOperators._
import scala.language.experimental.macros
import Wolfe._
import scala.annotation.StaticAnnotation

/**
 * @author Sebastian Riedel
 */
@Inlinable(classOf[LossesInlines])
object Losses {

  def perceptronLoss[T](data: Iterable[T])
                       (model: Vector => T => Double)
                       (predictor: T => T)
                       (weights: Vector): Double =
    sum { over(data) of (t => model(weights)(predictor(t)) - model(weights)(t)) }

}

class Inlinable(clz:Class[_]) extends StaticAnnotation

class LossesInlines {
  def perceptronLoss[T](c: Context)
                       (data: c.Expr[Iterable[T]])
                       (model: c.Expr[Vector => T => Double])
                       (predictor: c.Expr[T => T])
                       (weights: c.Expr[Vector]) = {
    c.universe.reify[Double] {
      sum { over(data.splice) of (t => model.splice(weights.splice)(predictor.splice(t)) - model.splice(weights.splice)(t)) }
    }
  }

}
