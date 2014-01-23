package scalapplcodefest.sbt

import scalapplcodefest._
import cc.factorie.WeightsSet
import cc.factorie.optimize.{Example, Trainer}
import cc.factorie.la.WeightsMapAccumulator
import cc.factorie.util.DoubleAccumulator
import java.io.PrintWriter
import scalapplcodefest.Wolfe.Objective.Adagrad

/**
 * @author Sebastian Riedel
 */
class DifferentiableArgminReplacer(val env: GeneratorEnvironment)
  extends CodeStringReplacer with WolfePatterns {

  this: Differentiator =>

  import env._
  import env.global._

  def differentiableAnnotationToFactorieLearningCall(annotation: AnnotationInfo, weightsSetId: String) = {
    val iterations = annotation.args(1) match {
      case Literal(Constant(i)) => i
      case _ => 5
    }
    println(annotation.args(0).tpe.safeToString)
    println(classOf[Adagrad].getName.replaceAll("\\$","."))

    val optimizer = annotation.args(0) match {
      case arg@Apply(adaGrad,List(Literal(Constant(rate))))
        if arg.tpe.safeToString == classOf[Adagrad].getName.replaceAll("\\$",".") =>
        s"new cc.factorie.optimize.AdaGrad($rate)"
      case _ => "new Perceptron"
    }
    s"new OnlineTrainer($weightsSetId, $optimizer, $iterations)"
  }


  def replace(tree: env.global.Tree, modification: ModifiedSourceText) = {
    val simplified = simplifyBlocks(tree)
    simplified match {
      case ApplyArgmin(_, _, _, _, Function(List(w1), Apply(obj, List(w2))), _)
        if w1.symbol == w2.symbol && obj.symbol.hasAnnotation(MarkerDifferentiable) =>
        val unrolled = simplifyBlocks(betaReduce(replaceMethods(obj)))
        unrolled match {
          case Function(List(w), ApplySum(_, _, data, _, Function(List(y_i), perInstance), _)) =>
            val indexId = "index"
            val weightsId = "weights"
            differentiate(perInstance, w.symbol, indexId, weightsId) match {
              case Some(gradientValue) =>
                val indentation = modification.indentationOfLineAt(tree.pos.start)
                val diffAnnotation = obj.symbol.getAnnotation(MarkerDifferentiable)
                val learnerConstructor = differentiableAnnotationToFactorieLearningCall(diffAnnotation.get, "_weightsSet")
                val replacement = ArgminByFactorieTrainer.generateCode(
                  data.symbol.name.toString, y_i.symbol.name.toString, indexId,
                  weightsId, gradientValue, true, learnerConstructor, indentation + 2)
                modification.replace(tree.pos.start, tree.pos.end, env.normalize(replacement))
                true
              case _ => false
            }
          case _ => false
        }
      case other =>
        false

    }
  }

}


object DifferentiableArgminReplacer {

  import scalapplcodefest.newExamples._

  def main(args: Array[String]) {
    val className = classOf[SumOfQuadraticFunctions].getName.replaceAll("\\.", "/")
    GenerateSources.generate(
      sourcePath = s"src/main/scala/$className.scala",
      replacers = List(g => new DifferentiableArgminReplacer(g) with SimpleDifferentiator))
  }
}


trait Differentiator extends InGeneratorEnvironment {

  import env.global._

  //this should return a string representation of (factorie gradient vector, objective value)
  def differentiate(objective: Tree, variable: Symbol,
                    indexIdentifier: String, weightIdentifier: String): Option[String]

}

trait SimpleDifferentiator extends Differentiator with WolfePatterns {

  import env.global._

  val toSparseFVector = "scalapplcodefest.sbt.FactorieConverter.toFactorieSparseVector"
  val printer = new env.SimplePrinter(new PrintWriter(ConsoleWriter))

  val conditionReplacer = new ConditionReplacer(env)

  def toFactorieObjective(tree: Tree, variable: Symbol, indexIdentifier: String, weightIdentifier: String): String = {

    def dotProductWithWeights(y: Symbol, arg: Tree) = s"${y.encodedName} => $weightIdentifier.dot($toSparseFVector($arg,$indexIdentifier))"

    tree match {
      case Function(List(y), DotProduct(arg1, arg2)) if arg1.symbol == variable => dotProductWithWeights(y.symbol, arg2)
      case Function(List(y), DotProduct(arg1, arg2)) if arg2.symbol == variable => dotProductWithWeights(y.symbol, arg1)
      case _ => sys.error(s"Can't convert $tree to factorie objective")
    }
  }

  //this should return a string representation of (factorie gradient vector, objective value)
  def differentiate(objective: env.global.Tree, variable: env.global.Symbol,
                    indexIdentifier: String, weightIdentifier: String) = {

    println(s"Differentiating $objective wrt $variable")

    objective match {
      case ApplyDoubleMinus(arg1, arg2) =>
        for (g1 <- differentiate(arg1, variable, indexIdentifier, weightIdentifier);
             g2 <- differentiate(arg2, variable, indexIdentifier, weightIdentifier)) yield
          s"{val (g1,v1) = $g1; val (g2,v2) = $g2; (g1 - g2, v1 - v2)}"

      case ApplyMax(se, types, dom, pred, obj@Function(List(y), body), num) =>
        val newDomPred = conditionReplacer.newDomainAndPredicate(dom.asInstanceOf[conditionReplacer.env.global.Tree], pred.asInstanceOf[conditionReplacer.env.global.Tree])
        val fObj = toFactorieObjective(obj, variable, indexIdentifier, weightIdentifier)
        val argmaxString = newDomPred match {
          case Some((newDom, newPred)) => s"argmax($newDom)($newPred)($fObj)($num)"
          case _ => s"argmax($dom)($pred)($fObj)($num)"
        }
        val argmaxSymbol = objective.symbol.owner.newValue(newTermName("_best"))
        val binding = Map(y.symbol -> Ident(argmaxSymbol))
        val atOptimum = env.substitute(body, binding)
        for (g <- differentiate(atOptimum, variable, indexIdentifier, weightIdentifier)) yield {
          s"{val ${argmaxSymbol.encodedName} = $argmaxString; $g}"
        }

      case DotProduct(arg1, arg2) if !arg1.exists(_.symbol == variable) && arg2.symbol == variable =>
        Some(s"{val _f = $toSparseFVector($arg1,$indexIdentifier); (_f,_f dot $weightIdentifier)}")

      case _ =>
        Some(s"($toSparseFVector(Wolfe.VectorZero,$indexIdentifier),0.0)")
    }

  }
}

object FactorieConverter {

  import Wolfe.{Vector => WVector}
  import scalapplcodefest.{Vector => FVector}

  def toFactorieSparseVector(vector: WVector, index: Index): FVector = {
    val sparse = new SparseVector(vector.size)
    for ((key, value) <- vector) sparse(index(Seq(key))) = value
    sparse
  }
  def toWolfeVector(fvector: FVector, index: Index): WVector = {
    val inverse = index.inverse()
    val map = for ((key, value) <- fvector.activeElements; inv <- inverse.get(key)) yield inv(0) -> value
    map.toMap
  }

}

object ArgminByFactorieTrainer {

  import scalapplcodefest.Vector

  def argmin[T](data: Seq[T],
                gradientValue: (Index, Vector) => (Vector, Double),
                trainerFor: WeightsSet => Trainer) = {
    val index = new Index
    val weightsSet = new WeightsSet
    val key = weightsSet.newWeights(new DenseVector(10000))
    val examples = for (instance <- data) yield new Example {
      def accumulateValueAndGradient(value: DoubleAccumulator, gradient: WeightsMapAccumulator) = {
        val weights = weightsSet(key).asInstanceOf[Vector]
        val (g, v) = gradientValue(index, weights)
        value.accumulate(v)
        gradient.accumulate(key, g, -1.0)
      }
    }
    val trainer = trainerFor(weightsSet)
    trainer.trainFromExamples(examples)
    weightsSet(key).asInstanceOf[Vector]
    ??? //convert to Wolfe.Vector
  }

  def generateCode(data: String, instanceVar: String, indexId: String, weightId: String, gradientValue: String,
                   newLines: Boolean = false,
                   learnerConstructor: String = "new OnlineTrainer(_weightsSet, new Perceptron, 5)",
                   indent: Int = 6) = {
    val raw = s"""{ //this code calls the factorie learner
      |import cc.factorie.WeightsSet
      |import cc.factorie.optimize.{Example, Trainer}
      |import cc.factorie.la.WeightsMapAccumulator
      |import cc.factorie.util.DoubleAccumulator
      |import cc.factorie.optimize.{Perceptron, OnlineTrainer}
      |import scalapplcodefest._
      |val $indexId = new Index
      |val _weightsSet = new WeightsSet
      |val _key = _weightsSet.newWeights(new DenseVector(10000))
      |val examples = for ($instanceVar <- $data) yield new Example {
      |  def accumulateValueAndGradient(value: DoubleAccumulator, gradient: WeightsMapAccumulator) = {
      |    val $weightId = _weightsSet(_key).asInstanceOf[Vector]
      |    val (g,v) = $gradientValue
      |    value.accumulate(v)
      |    gradient.accumulate(_key, g, -1.0)
      |  }
      |}
      |val _trainer = $learnerConstructor
      | _trainer.trainFromExamples(examples)
      |val fweights = _weightsSet(_key).asInstanceOf[Vector]
      |scalapplcodefest.sbt.FactorieConverter.toWolfeVector(fweights,$indexId)}
    """
    val withNewLines = if (newLines) raw.replaceAll(";", ";\n\\|") else raw
    withNewLines.replaceAll("\\|", Array.fill(indent)(" ").mkString("|", "", "")).stripMargin

  }
}

object RunOptimizedIris {

  import scalapplcodefest.newExamples._

  def main(args: Array[String]) {
    val optimizedClass = WolfeOptimizer.optimizeClass[() => Unit](
      classOf[Iris], List(
        env => new DifferentiableArgminReplacer(env) with SimpleDifferentiator,
        env => new ConditionReplacer(env)))
    val iris = optimizedClass.newInstance()
    println(iris)
    iris()

  }
}
