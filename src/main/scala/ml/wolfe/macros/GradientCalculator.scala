package ml.wolfe.macros

import ml.wolfe.Wolfe
import scala.reflect.macros.Context
import org.scalautils.{Bad, Good, Or}

/**
 * Calculates gradients and values of a function at a particular argument.
 * @author Sebastian Riedel
 */
trait GradientCalculator {
  def valueAndGradient(param: ml.wolfe.FactorieVector): (Double, ml.wolfe.FactorieVector)
}

trait MetaGradientCalculators[C <: Context] extends CodeRepository[C] with PatternRepository[C] {

  import context.universe._

  case class MetaGradientCalculator(className: TypeName, classDef: Tree)

  case class CantDifferentiate(term:Tree)

  def metaGradientCalculator(rhs: Tree, weightVar: Symbol, indexTree: Tree): MetaGradientCalculator Or CantDifferentiate = {
    rhs match {
      case x if !x.exists(_.symbol == weightVar) =>
        val className = newTypeName(context.fresh("ZeroGradientCalculator"))
        val classDef = q"""
          final class $className extends ml.wolfe.macros.GradientCalculator {
            def valueAndGradient(param: ml.wolfe.FactorieVector): (Double, ml.wolfe.FactorieVector) = {
              ($x,new ml.wolfe.SparseVector(0))
            }
          }
        """
        Good(MetaGradientCalculator(className,classDef))
      case Dot(arg1,arg2) if arg1.symbol == weightVar && !arg2.exists(_.symbol == weightVar) =>
        val className = newTypeName(context.fresh("DotGradientCalculator"))
        val classDef = q"""
          final class $className extends ml.wolfe.macros.GradientCalculator {
            val coefficient = ml.wolfe.FactorieConverter.toFactorieSparseVector($arg2,$indexTree)
            def valueAndGradient(param: ml.wolfe.FactorieVector): (Double, ml.wolfe.FactorieVector) = {
              (param dot coefficient, coefficient)
            }
          }
        """
        Good(MetaGradientCalculator(className,classDef))
      case ApplyMinus(arg1, arg2) =>
        for (g1 <- metaGradientCalculator(arg1, weightVar, indexTree);
             g2 <- metaGradientCalculator(arg2, weightVar, indexTree)) yield {
          val className = newTypeName(context.fresh("PlusGradientCalculator"))
          val classDef = q"""
          final class $className extends ml.wolfe.macros.GradientCalculator {
                ${g1.classDef}
                ${g2.classDef}
                val arg1 = new ${g1.className}
                val arg2 = new ${g2.className}
                def valueAndGradient(param: ml.wolfe.FactorieVector): (Double, ml.wolfe.FactorieVector) = {
                  val (v1,g1) = arg1.valueAndGradient(param)
                  val (v2,g2) = arg2.valueAndGradient(param)
                  ml.wolfe.util.LoggerUtil.debug("g1:" + ml.wolfe.FactorieConverter.toWolfeVector(g1,$indexTree))
                  ml.wolfe.util.LoggerUtil.debug("g2:" + ml.wolfe.FactorieConverter.toWolfeVector(g2,$indexTree))
                  (v1 - v2, g1 - g2)
                }}
          """
          MetaGradientCalculator(className, classDef)
        }
      case x => inlineOnce(x) match {
        case Some(inlined) => metaGradientCalculator(inlined,weightVar,indexTree)
        case None => Bad(CantDifferentiate(x))
      }

    }

  }
}

object GradientCalculator {

  import scala.language.experimental.macros


  def valueAndgradientAt(function: Wolfe.Vector => Double, argument: Wolfe.Vector): (Double,Wolfe.Vector) = macro valueAndgradientAtImpl

  def valueAndgradientAtImpl(c: Context)(function: c.Expr[Wolfe.Vector => Double], argument: c.Expr[Wolfe.Vector]) = {
    import c.universe._
    val helper = new ContextHelper[c.type](c) with MetaGradientCalculators[c.type]
    val q"($x) => $rhs" = helper.simplifyBlocks(function.tree)
    val index = q"_index"
    helper.metaGradientCalculator(rhs,x.symbol,index) match {
      case Good(calculator) =>
        val code = q"""
          val _index = new ml.wolfe.Index()
          ${calculator.classDef}
          val calculator = new ${calculator.className}
          val factorieArgument = ml.wolfe.FactorieConverter.toFactorieSparseVector(${argument.tree},_index)
          val (value,gradient) = calculator.valueAndGradient(factorieArgument)
          val wolfeResult = ml.wolfe.FactorieConverter.toWolfeVector(gradient, _index)
          (value,wolfeResult)
        """
        c.Expr[(Double,Wolfe.Vector)](code)
      case Bad(helper.CantDifferentiate(term)) =>
        c.error(c.enclosingPosition,"Can't calculate gradient for " + term)
        ??? //todo: I don't know what should be returned here---doesn't the compiler quit at this point?
    }
  }


}