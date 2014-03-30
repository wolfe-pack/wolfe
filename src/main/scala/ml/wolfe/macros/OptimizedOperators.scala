package ml.wolfe.macros

import ml.wolfe.{BruteForceOperators, Wolfe, Operators}
import scala.reflect.macros.Context
import Wolfe._
import org.scalautils.{Bad, Good}

/**
 * @author Sebastian Riedel
 */
object OptimizedOperators extends Operators {

  import scala.language.experimental.macros

  override def argmax[T, N: Ordering](overWhereOf: OverWhereOf[T, N]) = macro argmaxImpl[T, N]


  def argmaxImpl[T: c.WeakTypeTag, N: c.WeakTypeTag](c: Context)
                                                    (overWhereOf: c.Expr[OverWhereOf[T, N]])
                                                    (ord: c.Expr[Ordering[N]]) = {
    import c.universe._
    val helper = new ContextHelper[c.type](c) with OptimizedOperators[c.type]
    val result = helper.argmax(overWhereOf.tree)
    c.Expr[T](result)
  }

}

trait OptimizedOperators[C <: Context] extends MetaStructures[C]
                                               with MetaStructuredFactors[C]
                                               with Conditioner[C]
                                               with MetaGradientCalculators[C] {

  import context.universe._


  def inferenceCode(objRhs: Tree) = objRhs match {
    case q"$f(${ _ })" =>
      val t: Tree = f
      t.symbol.annotations.find(_.tpe.typeSymbol == wolfeSymbols.maxByInference) match {
        case Some(annotation) => q"${ annotation.scalaArgs.head }(_graph)"
        case None => q"ml.wolfe.MaxProduct(_graph,1)"
      }
    case _ => q"ml.wolfe.MaxProduct(_graph,1)"
  }


  def argmaxLinearModel(trees: OverWhereOfTrees): Tree = {
    val structName = newTermName(context.fresh("structure"))
    val meta = metaStructure(trees.over)
    val Function(List(objArg), objRhs) = simplifyBlocks(trees.of)
    val objMatcher = meta.matcher(rootMatcher(objArg.symbol, q"$structName", meta))
    val factors = metaStructuredFactor(objRhs, meta, objMatcher, linearModelInfo = LinearModelInfo(q"_index"))
    val inferCode = inferenceCode(objRhs)

    val structureDef = meta.classDef(newTermName("_graph"))

    val conditionCode = if (trees.where == EmptyTree) EmptyTree
    else {
      val Function(List(whereArg), whereRhs) = simplifyBlocks(trees.where)
      val whereMatcher = meta.matcher(rootMatcher(whereArg.symbol, q"$structName", meta))
      val conditioner = conditioning(whereRhs, whereMatcher)
      conditioner.code
    }
    val factorieWeights = factors.weightVector.map(
      w => q"ml.wolfe.FactorieConverter.toFactorieDenseVector($w,_index)"
    ).getOrElse(q"new ml.wolfe.DenseVector(0)")


    val code = q"""
      val _index = new ml.wolfe.Index()
      val _graph = new ml.wolfe.MPGraph
      $structureDef
      val $structName = new ${ meta.className }
      $conditionCode
      _graph.setupNodes()
      ${ factors.classDef }
      val factors = new ${ factors.className }($structName)
      _graph.build()
      val _factorieWeights = $factorieWeights
      _graph.weights = _factorieWeights
      $inferCode
      $structName.setToArgmax()
      $structName.value()
    """
    code
  }

  def argmaxByLearning(trees: OverWhereOfTrees): Tree = {
    if (trees.where != EmptyTree)
      context.error(context.enclosingPosition, "Can't learn with constraints on weights yet: " + trees.where)
    val q"($arg) => $rhs" = simplifyBlocks(trees.of)
    def toSum(tree:Tree):OverWhereOfTrees = tree match {
      case s@Sum(over,where,of,_) => OverWhereOfTrees(over,where,of)
      case s => inlineOnce(tree) match {
        case Some(inlined) => toSum(inlined)
        case None => OverWhereOfTrees(q"List(0)",EmptyTree,q"(i:Int) => $s")
      }
    }
    val sum = toSum(rhs)
    val q"($x) => $perInstanceRhs" = simplifyBlocks(sum.of)
    val instanceName = newTermName(context.fresh("_instance"))
    val indexName = newTermName(context.fresh("_index"))
    val weightsSet = newTermName(context.fresh("_weightsSet"))
    val key = newTermName(context.fresh("_key"))

    val replaced = transform(perInstanceRhs, {case i: Ident if i.symbol == x.symbol => Ident(instanceName)})

    metaGradientCalculator(replaced, arg.symbol, Ident(indexName)) match {
      case Good(calculator) =>
        val code = q"""
          import cc.factorie.WeightsSet
          import cc.factorie.la.WeightsMapAccumulator
          import cc.factorie.util.DoubleAccumulator
          import cc.factorie.optimize._
          import ml.wolfe.util.LoggerUtil
          import ml.wolfe._

          val $indexName = new Index
          val $weightsSet = new WeightsSet
          val $key = $weightsSet.newWeights(new ml.wolfe.DenseVector(10000))
          val examples = for ($instanceName <- ${sum.over}) yield new Example {
            ${ calculator.classDef }
            val gradientCalculator = new ${ calculator.className }
            def accumulateValueAndGradient(value: DoubleAccumulator, gradient: WeightsMapAccumulator) = {
              LoggerUtil.debug("Instance: " + $indexName)
              val weights = $weightsSet($key).asInstanceOf[FactorieVector]
              val (v, g) = gradientCalculator.valueAndGradient(weights)
              value.accumulate(v)
              gradient.accumulate($key, g, 1.0)
            }
          }
          val trainer = new OnlineTrainer($weightsSet, new Perceptron, 4)
          trainer.trainFromExamples(examples)
          ml.wolfe.FactorieConverter.toWolfeVector($weightsSet($key).asInstanceOf[FactorieVector], $indexName)
        """
        context.resetLocalAttrs(code)
      case Bad(CantDifferentiate(term)) =>
        context.error(context.enclosingPosition, "Can't calculate gradient for " + term)
        ??? //todo: I don't know what should be returned here---doesn't the compiler quit at this point?
    }
  }

  def argmax(overWhereOf: Tree): Tree = {

    val trees = overWhereOfTrees(overWhereOf)
    if (trees.over.symbol == wolfeSymbols.vectors) argmaxByLearning(trees) else argmaxLinearModel(trees)
  }


}