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
@Inlinable(classOf[LibraryExpressions])
object Library {

  def perceptronLoss[T](data: Iterable[T])
                       (model: Vector => T => Double)
                       (predictor: T => T)
                       (weights: Vector): Double =
    sum { over(data) of (t => model(weights)(predictor(t)) - model(weights)(t)) }

  def evidence[T](observed:T=>T)(t1:T)(t2:T) = observed(t1) == observed(t2)

}

class Inlinable(clz:Class[_]) extends StaticAnnotation

class LibraryExpressions {
  def perceptronLoss[T](c: Context)
                       (data: c.Expr[Iterable[T]])
                       (model: c.Expr[Vector => T => Double])
                       (predictor: c.Expr[T => T])
                       (weights: c.Expr[Vector]) = {
    c.universe.reify[Double] {
      sum { over(data.splice) of (t => model.splice(weights.splice)(predictor.splice(t)) - model.splice(weights.splice)(t)) }
    }
  }

  def evidence[T](c:Context)(observed:c.Expr[T=>T])(t1:c.Expr[T])(t2:c.Expr[T]) =
    c.universe.reify[Boolean]{
      observed.splice(t1.splice) == observed.splice(t2.splice)
    }

}
