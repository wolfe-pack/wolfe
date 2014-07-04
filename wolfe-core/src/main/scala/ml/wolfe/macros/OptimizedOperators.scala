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

  override def argmax[T, N: Ordering](dom: Iterable[T])(obj: T => N): T = macro argmaxImplNew[T, N]
//    override def logZ[T](dom: Iterable[T])(obj: T => Double): Double = macro logZImpl[T]
  override def argmin[T, N: Ordering](dom: Iterable[T])(obj: T => N): T = macro argminImplNew[T, N]
  override def map[A, B](dom: Iterable[A])(mapper: A => B): Iterable[B] = macro mapImplNew[A, B]

  def argmaxImplNew[T: c.WeakTypeTag, N: c.WeakTypeTag](c: Context)
                                                       (dom: c.Expr[Iterable[T]])
                                                       (obj: c.Expr[T => N])
                                                       (ord: c.Expr[Ordering[N]]) = {
    val helper = new ContextHelper[c.type](c) with OptimizedOperators[c.type]
    val trees = helper.builderTrees(dom.tree, obj.tree)
    //do not replace the code if inside another macro
    if (c.enclosingMacros.size > 1) {
      import c.universe._
      val code: Tree = q"${ trees.over }.filter(${ trees.where }).maxBy(${ trees.of })"
      c.Expr[T](code)
    } else {
      val result = helper.argmax(trees)
      c.Expr[T](c.resetLocalAttrs(result.combined))
    }
  }


  def argminImplNew[T: c.WeakTypeTag, N: c.WeakTypeTag](c: Context)
                                                       (dom: c.Expr[Iterable[T]])
                                                       (obj: c.Expr[T => N])
                                                       (ord: c.Expr[Ordering[N]]) = {
    import c.universe._
    val helper = new ContextHelper[c.type](c) with OptimizedOperators[c.type]
    val trees = helper.builderTrees(dom.tree, obj.tree)
    val result: Tree = helper.argmax(trees, q"-1.0").combined
    c.Expr[T](result)
  }


  def mapImplNew[A: c.WeakTypeTag, B: c.WeakTypeTag](c: Context)
                                                    (dom: c.Expr[Iterable[A]])
                                                    (mapper: c.Expr[A => B]) = {
    import c.universe._
    val helper = new ContextHelper[c.type](c) with OptimizedOperators[c.type]

    val trees = helper.builderTrees(dom.tree, EmptyTree, mapper.tree)

    val result: Tree = helper.map(trees)
    c.Expr[Iterable[B]](result)
  }

  def logZImpl[T: c.WeakTypeTag](c: Context)
                                (dom: c.Expr[Iterable[T]])
                                (obj: c.Expr[T => Double]) = {
//    val helper = new ContextHelper[c.type](c) with MetaGradientCalculators[c.type]
//    val gv = GradientCalculator.valueAndgradientAtImpl(c)(BruteForceOperators.logZ(dom.splice)(obj.splice))
//    val trees = helper.builderTrees(dom.tree, obj.tree)
//    //do not replace the code if inside another macro
//    if (c.enclosingMacros.size > 1) {
//      import c.universe._
//      val code: Tree = q"ml.wolfe.macros.BruteForceOperators.logZ(${ trees.over }.filter(${ trees.where }))(${ trees.of })"
//      c.Expr[Double](code)
//    } else {
//      val result = helper.argmax(trees)
//      c.Expr[Double](c.resetLocalAttrs(result.combined))
//    }
  }


}

trait LinearModelArgmaxCode[C <: Context] extends SymbolRepository[C] {

  import context.universe._

  def optimizeByInferenceCode(objRhs: Tree, graph: TermName): Tree = {
    def getCodeFromAnnotation(f: Tree): Tree =
      f.symbol.annotations.find(_.tpe.typeSymbol == wolfeSymbols.optByInference) match {
        case Some(annotation) => q"${ annotation.scalaArgs.head }($graph)"
        case None => q"ml.wolfe.BeliefPropagation($graph,1)"
      }
    objRhs match {
      case q"$f(${ _ })" => getCodeFromAnnotation(f)
      case q"$f(${ _ })(${ _ })" => getCodeFromAnnotation(f)
      case _ => q"ml.wolfe.BeliefPropagation($graph,1)"
    }
  }

  def logZByInferenceCode(objRhs: Tree, graph: TermName): Tree = {
    def getCodeFromAnnotation(f: Tree): Tree =
      f.symbol.annotations.find(_.tpe.typeSymbol == wolfeSymbols.logZByInference) match {
        case Some(annotation) => q"${ annotation.scalaArgs.head }($graph)"
        case None => q"ml.wolfe.BeliefPropagation.sumProduct(1)($graph)"
      }
    objRhs match {
      case q"$f(${ _ })" => getCodeFromAnnotation(f)
      case q"$f(${ _ })(${ _ })" => getCodeFromAnnotation(f)
      case _ => q"ml.wolfe.BeliefPropagation.sumProduct(1)($graph)"
    }
  }


}

trait OptimizedOperators[C <: Context] extends MetaStructures[C]
                                               with MetaStructuredFactors[C]
                                               with Conditioner[C]
                                               with MetaGradientCalculators[C]
                                               with LinearModelArgmaxCode[C] {

  import context.universe._


  def learningCode(objRhs: Tree, weightsSet: TermName): Tree = {
    def getCodeFromAnnotation(f: Tree): Tree = {
      f.symbol.annotations.find(_.tpe.typeSymbol == wolfeSymbols.optByLearning) match {
        case Some(annotation) => q"${ annotation.scalaArgs.head }($weightsSet)"
        case None => q"new OnlineTrainer($weightsSet, new Perceptron, 4)"
      }
    }
    objRhs match {
      //todo: generalize to allow any number of arguments
      case q"$f(${ _ })" =>
        getCodeFromAnnotation(f)
      case q"$f(${ _ })(${ _ })" =>
        getCodeFromAnnotation(f)
      case _ => q"new OnlineTrainer($weightsSet, new Perceptron, 4)"
    }
  }

  def map(trees: BuilderTrees): Tree = {
    val Function(List(mapperArg), _) = unwrapSingletonBlocks(trees.using)

    //we should do this until no more inlining can be done

    //todo: this is messy
    def transform(using: Tree) = transformAndCollect[List[Tree]](using, {
      case ArgmaxOperator(argmaxBuilder) =>
        val codeAndInit = argmaxLinearModel(argmaxBuilder)
        val initContainsMappingArg = codeAndInit.initialization.exists(_.exists(_.symbol == mapperArg.symbol))
        if (initContainsMappingArg) (codeAndInit.combined, Nil) else (codeAndInit.code, codeAndInit.initialization)
    })
    var (mapper, initCode) = transform(trees.using)
    var inlined = inlineOnce(mapper)
    while (inlined.isDefined) {
      val (m, i) = transform(inlined.get)
      initCode :::= i
      mapper = m
      inlined = inlineOnce(m)
    }

    val mapCall = trees.where match {
      case w if w == EmptyTree => q"${ trees.over }.map($mapper)"
      case w => q"${ trees.over }.filter($w).map($mapper)"
    }
    val flattened = initCode.flatten
    val code = q"""
      ..$flattened
      $mapCall
    """
    context.resetLocalAttrs(code)
  }

  case class CodeAndInitialization(code: Tree, initialization: List[Tree]) {
    def all = initialization :+ code
    def combined = q"{..$all}"
  }

  def argmaxLinearModel(trees: BuilderTrees, scaling: Tree = q"1.0"): CodeAndInitialization = {
    val structName = newTermName(context.fresh("structure"))
    val meta = metaStructure(trees.over)
    val Function(List(objArg), rawObjRhs) = blockToFunction(unwrapSingletonBlocks(trees.of))
    //todo: scaling should happen on a per factor basis (challenge: negating special-purpose potentials)
    val objRhs = if (scaling.equalsStructure(q"1.0")) rawObjRhs else q"$scaling * $rawObjRhs"
    val objMatcher = meta.matcher(rootMatcher(objArg.symbol, q"$structName", meta))
    val factors = metaStructuredFactor(FactorGenerationInfo(objRhs, meta, objMatcher, linearModelInfo = LinearModelInfo(q"_index")))
    val inferCode = optimizeByInferenceCode(objRhs, newTermName("_graph"))

    val structureDef = meta.classDef(newTermName("_graph"))

    val conditionCode = if (trees.where == EmptyTree) EmptyTree
    else {
      val Function(List(whereArg), whereRhs) = simplifyBlock(unwrapSingletonBlocks(trees.where))
      val whereMatcher = meta.matcher(rootMatcher(whereArg.symbol, q"$structName", meta))
      val conditioner = conditioning(whereRhs, whereMatcher)
      conditioner.code
    }
    val factorieWeights = factors.weightVector.map(
      w => q"ml.wolfe.FactorieConverter.toFactorieDenseVector($w,_index)"
    ).getOrElse(q"new ml.wolfe.DenseVector(0)")

    val initialization = List(
      q"val _index = new ml.wolfe.DefaultIndex()",
      q"val _factorieWeights = $factorieWeights")

    val code = q"""
      val _graph = new ml.wolfe.FactorGraph
      $structureDef
      val $structName = new ${ meta.className }
      $conditionCode
      _graph.setupNodes()
      ${ factors.classDef }
      val factors = new ${ factors.className }($structName)
      _graph.build()
      _graph.weights = _factorieWeights
      $inferCode
      $structName.setToArgmax()
      $structName.value()
    """
    CodeAndInitialization(context.resetLocalAttrs(code), initialization)
  }

  //todo: this may be replaced by simplifyBlocks / normalize at some point?
  def blockToFunction(tree: Tree): Tree = tree match {
    case f: Function => f
    case Block(stats, Function(args, body)) => {
      val (newBody, newStats) = stats.foldRight(body -> List.empty[Tree]) {
        (stat, result) => stat match {
          case v: ValDef => transform(result._1, { case i: Ident if i.symbol == v.symbol => v.rhs }) -> result._2
        }
      }
      Function(args, Block(newStats, newBody))
    }
    case _ => context.error(context.enclosingPosition, s"Can't turn $tree into function"); ???
  }

  def argmaxByLearning(trees: BuilderTrees, scaling: Tree = q"1.0"): Tree = {
    if (trees.where != EmptyTree)
      context.error(context.enclosingPosition, "Can't learn with constraints on weights yet: " + trees.where)
    val q"($arg) => $rhs" = simplifyBlock(trees.of)
    def toSum(tree: Tree): BuilderTrees = tree match {
      case s@Sum(BuilderTrees(over, where, of, _, _)) => BuilderTrees(over, where, of)
      case s => inlineOnce(tree) match {
        case Some(inlined) => toSum(inlined)
        case None => BuilderTrees(q"List(0)", EmptyTree, q"(i:Int) => $s")
      }
    }
    val sum = toSum(rhs)
    val q"($x) => $perInstanceRhs" = unwrapSingletonBlocks(sum.of)
    val instanceName = newTermName(context.fresh("_instance"))
    val indexName = newTermName(context.fresh("_index"))
    val weightsSet = newTermName(context.fresh("_weightsSet"))
    val key = newTermName(context.fresh("_key"))
    val learner = learningCode(rhs, weightsSet)

    val replaced = transform(perInstanceRhs, {
      case i: Ident if i.symbol == x.symbol => Ident(instanceName) //treeCopy.Ident(i,instanceName)
    })
    //todo: this doesn't work
    val exampleDom = iterableArgumentType(context.typeCheck(context.resetLocalAttrs(sum.over)))

    //metaGradientCalculator(replaced, arg.symbol, Ident(indexName)) match {
    metaGradientCalculator(replaced, arg.symbol, Ident(indexName)) match {
      case Good(calculator) =>
        val classDef = calculator.classDef
        val code = q"""
          import cc.factorie.WeightsSet
          import cc.factorie.Weights
          import cc.factorie.la.WeightsMapAccumulator
          import cc.factorie.util.DoubleAccumulator
          import cc.factorie.optimize._
          import ml.wolfe.util.LoggerUtil
          import ml.wolfe._
          import scala.language.reflectiveCalls

          val $indexName = new ml.wolfe.DefaultIndex()
          val $weightsSet = new WeightsSet
          ml.wolfe.util.LoggerUtil.info("Creating examples ...")
          val examples = ml.wolfe.util.Timer.time("examples") { ${ sum.over }.map(($instanceName:$exampleDom) => new Example {
              ${ classDef }
              var _key:Weights = null
              val gradientCalculator = new ${ calculator.className }
              def accumulateValueAndGradient(value: DoubleAccumulator, gradient: WeightsMapAccumulator) = {
                LoggerUtil.debug("Instance: " + $indexName)
                val weights = $weightsSet(_key).asInstanceOf[FactorieVector]
                val (v, g) = gradientCalculator.valueAndGradient(weights)
                value.accumulate($scaling * v)
                gradient.accumulate(_key, g, $scaling)
              }
            })
          }
          ml.wolfe.util.LoggerUtil.info(s"Speed: "  +  (1000 * examples.size.toDouble / ml.wolfe.util.Timer.reported("examples")) + " examples/sec")
          val $key = $weightsSet.newWeights(new ml.wolfe.DenseVector($indexName.size))
          examples.foreach(example => example._key = $key)
          val trainer = $learner
          ml.wolfe.util.LoggerUtil.info("Starting to optimize ...")
          trainer.trainFromExamples(examples)
          ml.wolfe.FactorieConverter.toWolfeVector($weightsSet($key).asInstanceOf[FactorieVector], $indexName)
        """
        context.resetLocalAttrs(code)
      case Bad(CantDifferentiate(term)) =>
        context.error(context.enclosingPosition, "Can't calculate gradient for " + term)
        ??? //todo: I don't know what should be returned here---doesn't the compiler quit at this point?
    }
  }

  def argmax(trees: BuilderTrees, scaling: Tree = q"1.0"): CodeAndInitialization = {

    //todo: deal with scaling in linear model as well
    if (trees.over.symbol == wolfeSymbols.vectors)
      CodeAndInitialization(argmaxByLearning(trees, scaling), Nil)
    else {
      argmaxLinearModel(trees, scaling)
    }
  }

  trait IsmorphicFactorGraph[T] {
    def graph = structure.graph
    def structure: Structure[T]
  }

  def fg[T, N](overWhereOf: Builder[T, N]): IsmorphicFactorGraph[T] = null

}