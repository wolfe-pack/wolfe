package ml.wolfe.macros

import scala.reflect.macros.Context

/**
 * @author Sebastian Riedel
 */
trait CodeOptimizer[C <: Context] extends HasContext[C] with CodeRepository[C] with PatternRepository[C] {

  import context.universe._

  def toOptimizedFactorieVector(wolfeVector: Tree, index: Tree): Tree = wolfeVector match {
    case Sum(BuilderTrees(dom, filter, obj, _)) if filter == EmptyTree =>
      val Function(List(arg),body) = normalize(obj)
      body match {
        case q"ml.wolfe.Wolfe.oneHot($oneHotIndex,$oneHotValue)" =>
          val code = q"""
            val dom = $dom
            val result = new cc.factorie.la.GrowableSparseTensor1(dom)
            for (${arg.name} <- dom) {
              result +=($index.apply(Seq($oneHotIndex)),$oneHotValue)
            }
            result
          """
          context.resetAllAttrs(code)
        case _ => q"ml.wolfe.FactorieConverter.toFreshFactorieSparseVector($wolfeVector,$index)"
      }
    case ApplyPlus(arg1,arg2) =>
      val arg1Vector = toOptimizedFactorieVector(arg1,index)
      val arg2Vector = toOptimizedFactorieVector(arg2,index)
      val code = q"""
        val result = new cc.factorie.la.SparseTensor1($arg1.size + $arg2.size)
        result += $arg1Vector
        result += $arg2Vector
        result
      """
      code
    case q"ml.wolfe.Wolfe.oneHot($oneHotIndex,$oneHotValue)" =>
      q"$index.toCachedFactorieOneHotVector($oneHotIndex,$oneHotValue)"

    case other => inlineOnce(other) match {
      case Some(inlined) => toOptimizedFactorieVector(inlined, index)
      case _ => q"ml.wolfe.FactorieConverter.toFreshFactorieSparseVector($other,$index)"
    }
  }

  def optimizeFactorieVector(factorieVector: Tree) = factorieVector match {
    case q"ml.wolfe.FactorieConverter.toFreshFactorieSparseVector($wolfeVector,$index)" =>
      toOptimizedFactorieVector(wolfeVector,index)
    case _ => factorieVector
  }


}