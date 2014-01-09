package scalapplcodefest.sbt

import scala.tools.nsc.Global
import scalapplcodefest._
import cc.factorie.WeightsSet
import cc.factorie.optimize.{Example, Trainer}
import cc.factorie.la.WeightsMapAccumulator
import cc.factorie.util.DoubleAccumulator

/**
 * @author Sebastian Riedel
 */
class ArgminByFactorieTrainerReplacer(val global: Global)
  extends CodeStringReplacer with WolfePatterns {

  this: Differentiator =>

  import global._

  def replace(tree: global.Tree, modification: ModifiedSourceText) = {
    //assume a sum
    tree match {
      case ApplyArgmin(_, _, Function(List(w), ApplySum(_, data, Function(List(y_i), perInstance), _))) =>
        val indexId = "index"
        val weightsId = "weights"
        differentiate(perInstance, w.symbol, indexId, weightsId) match {
          case Some(gradientValue) =>
            val replacement = ArgminByFactorieTrainer.generateCode(
              data.symbol.name.toString, y_i.symbol.name.toString, indexId, weightsId, gradientValue)
            modification.replace(tree.pos.start, tree.pos.end, replacement)
            true
          case _ => false
        }

      case _ => false

    }
  }
}


object ArgminByFactorieTrainerReplacer {
  def main(args: Array[String]) {
    GenerateSources.generate(
      sourcePath = "src/main/scala/scalapplcodefest/sbt/SumOfQuadraticFunctionsExample.scala",
      replacers = List(g => new ArgminByFactorieTrainerReplacer(g) with SimpleDifferentiator))
  }
}


trait Differentiator extends HasGlobal {

  //this should return a string representation of (factorie gradient vector, objective value)
  def differentiate(objective: global.Tree, variable: global.Symbol,
                    indexIdentifier: String, weightIdentifier: String): Option[String]

}

trait SimpleDifferentiator extends Differentiator {

  //this should return a string representation of (factorie gradient vector, objective value)
  def differentiate(objective: global.Tree, variable: global.Symbol,
                    indexIdentifier: String, weightIdentifier: String) = {
    println(s"Differentiating $objective wrt $variable")
    val toSparseFVector = "scalapplcodefest.sbt.FactorieConverter.toFactorieSparseVector"
    Some(s"($toSparseFVector(Wolfe.VectorZero,$indexIdentifier),0.0)")
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
    val map = for ((key, value) <- fvector.activeElements) yield inverse(key)(0) -> value
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

  def generateCode(data: String, instanceVar: String, indexId: String, weightId: String, gradientValue: String) =
    s"""\n
      |//this code calls the factorie learner
      |{
      |    import cc.factorie.WeightsSet
      |    import cc.factorie.optimize.{Example, Trainer}
      |    import cc.factorie.la.WeightsMapAccumulator
      |    import cc.factorie.util.DoubleAccumulator
      |    import cc.factorie.optimize.{Perceptron, OnlineTrainer}
      |    import scalapplcodefest._
      |    val $indexId = new Index
      |    val _weightsSet = new WeightsSet
      |    val _key = _weightsSet.newWeights(new DenseVector(10000))
      |    val examples = for ($instanceVar <- $data) yield new Example {
      |      def accumulateValueAndGradient(value: DoubleAccumulator, gradient: WeightsMapAccumulator) = {
      |        val $weightId = _weightsSet(_key).asInstanceOf[Vector]
      |        val (g,v) = $gradientValue
      |        value.accumulate(v)
      |        gradient.accumulate(_key, g, -1.0)
      |      }
      |    }
      |    val _trainer = new OnlineTrainer(_weightsSet, new Perceptron, 5)
      |    _trainer.trainFromExamples(examples)
      |    val fweights = _weightsSet(_key).asInstanceOf[Vector]
      |    scalapplcodefest.sbt.FactorieConverter.toWolfeVector(fweights,$indexId)
      |}
    """.stripMargin

}

