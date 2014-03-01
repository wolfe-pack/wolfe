package ml.wolfe.macros

import scala.reflect.macros.Context
import cc.factorie.WeightsSet
import cc.factorie.optimize.{Perceptron, OnlineTrainer, Trainer}

/**
 * @author Sebastian Riedel
 */
trait GradientBasedMinimizationHelper[C <: Context] {
  this: MacroHelper[C] =>

  import context.universe._

  case class MetaGradientBasedMinimizer(dom: Tree, obj: Tree) {

    //todo: do something with dom
    val instanceVariableName = newTermName("_instance")
    val indexVariableName    = newTermName("_index")

    val normalizedObj = replaceVals(betaReduce(replaceMethods(simplifyBlocks(obj), defs)), noArgDefDefs)

    //convert objective into sum if not already a sum
    val sum: Tree = normalizedObj match {
      case s@q"(${_}) => sum(${_})(${_})(${_})" => s
      case q"($weights) => $rhs" => q"($weights) => sum(Seq(0))(_ => true)($rhs)" //todo: add numeric
    }

    //now get per-instance objective and function argument
    val q"($weights) => sum($data)($pred)(($arg) => $perInstance)" = sum

    //replace instance variable in client code with instance variable that we use in factorie code
    val replaced: Tree = transform(perInstance, {
      case i: Ident if i.symbol == arg.symbol => Ident(instanceVariableName)
    })

    //val wolfe = rootMirror.getModuleByName(newTermName("ml.wolfe.Wolfe"))

    //check annotation on original objective
    val trainer = simplifyBlocks(obj) match {
      case q"(${_}) => $rhs(${_})" =>
        println(rhs)
        val t: Tree = rhs
        t.symbol.annotations.collectFirst({
          //todo: use tpe symbol and check against MinByDescent symbol
          case Annotation(tpe, List(arg), _) if tpe.typeSymbol.name.encoded == "MinByDescent" => arg
        }) match {
          case Some(arg) => context.Expr[WeightsSet => Trainer](arg)
          case _ => reify((w: WeightsSet) => new OnlineTrainer(w, new Perceptron, 5))
        }
      case _ => reify((w: WeightsSet) => new OnlineTrainer(w, new Perceptron, 5))
    }

    val trainingCode = for (gradientCalculator <- generateGradientCalculatorClass(replaced, weights, indexVariableName)) yield {
      val generator = context.Expr[GradientCalculator](context.resetAllAttrs(gradientCalculator))
      //      val trainerFor = reify((w: WeightsSet) => new OnlineTrainer(w, new Perceptron, 5))
      val trainingData = context.Expr[Iterable[Any]](data)
      val code = generateFactorieCode(generator, trainer, trainingData)
      code
    }

    //todo: we might want to replace _instance in the code with a fresh variable name


    //generate gradient calculator for arguments in the sum

    def generateFactorieCode[T](gradientCalculatorGenerator: Expr[GradientCalculator],
                                trainerFor: Expr[WeightsSet => Trainer],
                                trainingData: Expr[Iterable[T]], scale: Expr[Double] = context.literal(-1.0)) = reify({
      import cc.factorie.WeightsSet
      import cc.factorie.la.WeightsMapAccumulator
      import cc.factorie.util.DoubleAccumulator
      import cc.factorie.optimize._
      import ml.wolfe._

      val _index = new Index
      val weightsSet = new WeightsSet
      val key = weightsSet.newWeights(new DenseVector(10000))
      val examples = for (_instance <- trainingData.splice) yield new Example {
        val gradientCalculator = gradientCalculatorGenerator.splice
        def accumulateValueAndGradient(value: DoubleAccumulator, gradient: WeightsMapAccumulator) = {
          println("Instance: " + _instance)
          val weights = weightsSet(key).asInstanceOf[FactorieVector]
          val (v, g) = gradientCalculator.valueAndGradient(weights)
          value.accumulate(v)
          gradient.accumulate(key, g, scale.splice)
        }
      }
      val trainer = trainerFor.splice(weightsSet)
      trainer.trainFromExamples(examples)
      FactorieConverter.toWolfeVector(weightsSet(key).asInstanceOf[FactorieVector], _index)
    })

  }

  trait MetaGradientCalculator {
    def className: TypeName
    def construct: Tree
  }

  //todo: currently derivation happens on the untyped tree. This creates issues in the presence of implicits
  //now create a gradient calculator class
  def generateGradientCalculatorClass(term: Tree, weightVar: ValDef,
                                      indexName: TermName): Option[Tree] = {
    term match {
      case ApplyMinus(arg1, arg2) =>
        for (g1 <- generateGradientCalculatorClass(arg1, weightVar, indexName);
             g2 <- generateGradientCalculatorClass(arg2, weightVar, indexName)) yield
          q"""new GradientCalculator {
                val arg1 = $g1
                val arg2 = $g2
                def valueAndGradient(param: ml.wolfe.FactorieVector): (Double, ml.wolfe.FactorieVector) = {
                  val (v1,g1) = arg1.valueAndGradient(param)
                  val (v2,g2) = arg2.valueAndGradient(param)
                  println("g1:" + FactorieConverter.toWolfeVector(g1,$indexName))
                  println("g2:" + FactorieConverter.toWolfeVector(g2,$indexName))
                  (v1 - v2, g1 - g2)
                }
              }
          """

      case DotProduct(arg1, arg2) if !arg1.exists(_.symbol == weightVar.symbol) && arg2.symbol == weightVar.symbol =>
        val cleaned = arg1 // context.typeCheck(context.resetAllAttrs(arg1))
        Some( q"""new GradientCalculator {
          val coefficient = FactorieConverter.toFactorieSparseVector($cleaned,$indexName)
          def valueAndGradient(param: ml.wolfe.FactorieVector): (Double, ml.wolfe.FactorieVector) = {
            (coefficient dot param,coefficient)
        }}""")
      case q"max($sampleSpace)($pred)($obj)" =>
        //create MPGraph
        val typed = context.typeCheck(sampleSpace)
        val metaData = MetaStructuredGraph(typed, pred, obj, {
          case i: Ident => i.symbol == weightVar.symbol
          case _ => false
        })

        //todo: this should be
        val maxBy:Tree = getMaxByProcedure(obj)

        //need to replace occurences of weightVariable in objective with toWolfeVector(param)
        Some( q"""new GradientCalculator {
          ${metaData.classDef}
          val _graph = new ${metaData.graphClassName}($indexName)
          def valueAndGradient(param: ml.wolfe.FactorieVector): (Double, ml.wolfe.FactorieVector) = {
            _graph.${metaData.mpGraphName}.weights = param
            $maxBy(_graph.${metaData.mpGraphName})
            (_graph.${metaData.mpGraphName}.value,_graph.${metaData.mpGraphName}.gradient)
        }}""")
      //        Some(q"???")
      case _ => None
    }
  }

}

trait GradientCalculator {
  def valueAndGradient(param: ml.wolfe.FactorieVector): (Double, ml.wolfe.FactorieVector)
}

