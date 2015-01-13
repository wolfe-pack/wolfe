package ml.wolfe.macros

import ml.wolfe.{FactorieVector, Wolfe}
import scala.reflect.macros.Context
import org.scalautils.{Bad, Good, Or}
import cc.factorie.la.SparseTensor1

/**
 * Calculates gradients and values of a function at a particular argument.
 * @author Sebastian Riedel
 */
trait GradientCalculator {
  def valueAndGradient(param: ml.wolfe.FactorieVector): (Double, ml.wolfe.FactorieVector)
}

trait MetaGradientCalculators[C <: Context] extends MetaStructures[C]
                                                    with Conditioner[C]
                                                    with MetaStructuredFactors[C]
                                                    with CodeOptimizer[C]
                                                    with LinearModelArgmaxCode[C] {

  import context.universe._

  case class MetaGradientCalculator(className: TypeName, classDef: Tree)

  case class CantDifferentiate(term: Tree)

  def constantGradientCalculator(term: Tree) = {
    val className = newTypeName(context.fresh("ZeroGradientCalculator"))
    val classDef = q"""
          final class $className extends ml.wolfe.macros.GradientCalculator {
            def valueAndGradient(param: ml.wolfe.FactorieVector): (Double, ml.wolfe.FactorieVector) = {
              ($term,new ml.wolfe.SparseVector(0))
            }
          }
        """
    MetaGradientCalculator(className, classDef)

  }
  def linearGradientCalculator(coefficient: Tree, indexTree: Tree) = {
    val className = newTypeName(context.fresh("DotGradientCalculator"))
    val factorieCoefficient = toOptimizedFactorieVector(coefficient, indexTree)
    //ml.wolfe.FactorieConverter.toFreshFactorieSparseVector($coefficient,$indexTree)
    val classDef = q"""
          final class $className extends ml.wolfe.macros.GradientCalculator {
            val coefficient = $factorieCoefficient
            def valueAndGradient(param: ml.wolfe.FactorieVector): (Double, ml.wolfe.FactorieVector) = {
              //println(value + " vs " + expected)
              (param dot coefficient, coefficient)
            }
          }
        """
    //val value = param dot coefficient
    //val expected = $coefficient dot ml.wolfe.FactorieConverter.toWolfeVector(param,$indexTree)
    MetaGradientCalculator(className, classDef)
  }

  def binaryOperatorGradientCalculator(arg1: Tree, arg2: Tree, weightVar: Symbol, indexTree: Tree,
                                       valueCombiner: (Tree, Tree) => Tree,
                                       gradientCombiner: (Tree, Tree) => Tree) = {
    for (g1 <- metaGradientCalculator(arg1, weightVar, indexTree);
         g2 <- metaGradientCalculator(arg2, weightVar, indexTree)) yield {
      val className = newTypeName(context.fresh("BinaryOperatorGradientCalculator"))
      val value = valueCombiner(q"v1", q"v2")
      val gradient = gradientCombiner(q"g1", q"g2")
      val classDef = q"""
          final class $className extends ml.wolfe.macros.GradientCalculator {
                ${ g1.classDef }
                ${ g2.classDef }
                val arg1 = new ${ g1.className }
                val arg2 = new ${ g2.className }
                def valueAndGradient(param: ml.wolfe.FactorieVector): (Double, ml.wolfe.FactorieVector) = {
                  val (v1,g1) = arg1.valueAndGradient(param)
                  val (v2,g2) = arg2.valueAndGradient(param)
                  ($value, $gradient)
                }}
          """
      MetaGradientCalculator(className, classDef)
    }
  }

  def addFactorieVectors(arg1: Tree, arg2: Tree) = {
    q"""
      val result = new cc.factorie.la.SparseTensor1($arg1.size + $arg2.size)
      result += $arg1
      result += $arg2
      result
    """
  }

  def subtractFactorieVectors(arg1: Tree, arg2: Tree) = {
    q"""
      ml.wolfe.macros.FactorieVectorHelper.subtract($arg1,$arg2)
    """
  }


  def inferenceGradientCalculator(dom:Tree,where:Tree,obj:Tree,indexTree:Tree,
                                  inferenceCode:(Tree,TermName) => Tree = optimizeByInferenceCode) = {
    val structName = newTermName(context.fresh("structure"))
    val meta = metaStructure(dom)
    val Function(List(objArg), objRhs) = unwrapSingletonBlocks(obj)

    val conditionerCode = if (where != EmptyTree) {
      val Function(List(whereArg), whereRhs) = simplifyBlock(unwrapSingletonBlocks(where))
      val whereMatcher = meta.matcher(rootMatcher(whereArg.symbol, q"$structName", meta))
      val conditioner = conditioning(whereRhs, whereMatcher)
      conditioner.code
    } else EmptyTree

    val objMatcher = meta.matcher(rootMatcher(objArg.symbol, q"$structName", meta))
    val factors = metaStructuredFactor(FactorGenerationInfo(objRhs, meta, objMatcher, linearModelInfo = LinearModelInfo(indexTree)))
    val structureDef = meta.classDef(newTermName("_graph"))
    val className = newTypeName(context.fresh("MaxGradientCalculator"))
    val inferCode = inferenceCode(objRhs, newTermName("_graph"))
    val classDef = q"""
          final class $className extends ml.wolfe.macros.GradientCalculator {
            val _graph = new ml.wolfe.FactorGraph
            $structureDef
            val $structName = new ${ meta.className }
            $conditionerCode
            _graph.setupNodes()
            ${ factors.classDef }
            val factors = new ${ factors.className }($structName)
            _graph.build()
            def valueAndGradient(param: ml.wolfe.FactorieVector): (Double, ml.wolfe.FactorieVector) = {
              _graph.weights = param
              $inferCode
              (_graph.value,_graph.gradient)
            }
          }
        """
    /*  code to test score

            val objWithFactorieWeights = transform(obj, {
              case i:Ident if i.symbol == weightVar => q"ml.wolfe.FactorieConverter.toWolfeVector(param,$indexTree)"
            })
          $structName.setToArgmax()
          val guess = $structName.value()
          val score = $objWithFactorieWeights(guess)
          println(score + " vs " + _graph.value)

     */
    Good(MetaGradientCalculator(className, context.untypecheck(classDef)))

  }

  def metaGradientCalculator(rhs: Tree, weightVar: Symbol, indexTree: Tree): MetaGradientCalculator Or CantDifferentiate = {
    rhs match {
      case x if !x.exists(_.symbol == weightVar) =>
        Good(constantGradientCalculator(x))
      case Dot(arg1, arg2) if arg1.symbol == weightVar && !arg2.exists(_.symbol == weightVar) =>
        Good(linearGradientCalculator(arg2, indexTree))
      case Dot(arg1, arg2) if arg2.symbol == weightVar && !arg1.exists(_.symbol == weightVar) =>
        Good(linearGradientCalculator(arg1, indexTree))
      case ApplyDoubleMinus(arg1, arg2) =>
        binaryOperatorGradientCalculator(arg1, arg2, weightVar, indexTree, (v1, v2) => q"$v1 - $v2", subtractFactorieVectors)
      case ApplyDoublePlus(arg1, arg2) =>
        binaryOperatorGradientCalculator(arg1, arg2, weightVar, indexTree, (v1, v2) => q"$v1 + $v2", addFactorieVectors)
      case DoubleMax(BuilderTrees(dom, where, obj, _,_)) =>
        inferenceGradientCalculator(dom,where,obj,indexTree)
      case LogZ(BuilderTrees(dom, where, obj, _,_)) =>
        inferenceGradientCalculator(dom,where,obj,indexTree,inferenceCode = logZByInferenceCode)
      //Bad(CantDifferentiate(rhs))
      case x => inlineOnce(x) match {
        case Some(inlined) => metaGradientCalculator(inlined, weightVar, indexTree)
        case None => Bad(CantDifferentiate(x))
      }

    }

  }
}

object GradientCalculator {

  import scala.language.experimental.macros

  def valueAndgradientAt(function: Wolfe.Vector => Double, argument: Wolfe.Vector): (Double, Wolfe.Vector) = macro valueAndgradientAtImpl

  def valueAndgradientAtImpl(c: Context)(function: c.Expr[Wolfe.Vector => Double], argument: c.Expr[Wolfe.Vector]) = {
    import c.universe._
    //todo: needed here for q"" expressions, but gets optimized away by Intellij
    val helper = new ContextHelper[c.type](c) with MetaGradientCalculators[c.type]
    val q"($x) => $rhs" = helper.unwrapSingletonBlocks(function.tree)
    val index = q"_index"
    helper.metaGradientCalculator(rhs, x.symbol, index) match {
      case Good(calculator) =>
        val code = q"""
          val _index = new ml.wolfe.DefaultIndex()
          ${ calculator.classDef }
          val calculator = new ${ calculator.className }
          val factorieArgument = ml.wolfe.FactorieConverter.toFactorieDenseVector(${ argument.tree },_index)
          val (value,gradient) = calculator.valueAndGradient(factorieArgument)
          val wolfeResult = ml.wolfe.FactorieConverter.toWolfeVector(gradient, _index)
          (value,wolfeResult)
        """
        c.Expr[(Double, Wolfe.Vector)](code)
      case Bad(helper.CantDifferentiate(term)) =>
        c.error(c.enclosingPosition, "Can't calculate gradient for " + term)
        ??? //todo: I don't know what should be returned here---doesn't the compiler quit at this point?
    }
  }


}

object FactorieVectorHelper {
  def subtract(arg1: FactorieVector, arg2: FactorieVector) = {
    val result = new cc.factorie.la.SparseTensor1(arg1.size + arg2.size)
    result += arg1
    result -= arg2
    result

  }
}