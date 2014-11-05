package ml.wolfe.apps.factorization

import ml.wolfe.apps.factorization.EntityAwareEvaluation.Entity
import ml.wolfe.apps.factorization.FactorizationUtil.{PredictedFact, Row}
import ml.wolfe.util.Util

import scala.collection.mutable

/**
 * @author Sebastian Riedel
 */
class EntityAwarePredictor(val embeddings: ProbLogicEmbeddings, val entities: Map[Any, Entity]) {

  val distanceCache = new mutable.HashMap[(String, String), Double]()

  def closest(candidates: Iterable[String], target: String) = {
    if (candidates.isEmpty) ("NA", Double.PositiveInfinity)
    else
      candidates.map(pred => {
        val dist = distanceCache.getOrElseUpdate(pred -> target,
          embeddings.embeddings(target).distance(embeddings.embeddings(pred)))
        pred -> dist
      }).minBy(_._2)
  }

  def farthest(candidates: Iterable[String], target: String) = {
    if (candidates.isEmpty) ("NA", Double.PositiveInfinity)
    else
      candidates.map(pred => {
        val dist = distanceCache.getOrElseUpdate(pred -> target,
          embeddings.embeddings(target).distance(embeddings.embeddings(pred)))
        pred -> dist
      }).maxBy(_._2)
  }

  def predictAll(row: Row, targetRelations:Seq[String], useFilter:Boolean = true) = {
    targetRelations.map(predict(row,_,useFilter))
  }

  import EntityAwareEvaluation._

  def predict(row: Row, target: String, useFilter:Boolean = true) = {
    val arg1 = entities(row.arg1)
    val arg2 = entities(row.arg2)

    val targetEmbedding = embeddings.embeddings(target)

    def filterObs(obs:Iterable[String]) = if (useFilter) obs.filter(targetEmbedding.observationFilter) else obs
    def asProb(pair:(String,Double)) = pair.copy(_2 = Util.sig(targetEmbedding.bias - pair._2))

    //find best unary predicate for arg1
    val arg1Result = closest(filterObs(arg1.asArg1), target)
    //find best unary predicate for arg2
    val arg2Result = closest(filterObs(arg2.asArg2), target)
    //find best binary predicate as observation
    val relResult = closest(filterObs(row.relations.view.map(_._1)), target)

    val (predictor, score) = Iterator(arg1Result, arg2Result, relResult).maxBy(_._2)

    val prob = Util.sig(targetEmbedding.bias - score)
    EntityAwarePrediction(
      PredictedFact(row, target, prob), predictor,
      asProb(arg1Result), asProb(arg2Result), asProb(relResult)
    )
  }

}

case class EntityAwarePrediction(fact: PredictedFact, predictor: String,
                                 arg1Result: (String, Double), arg2Result: (String, Double), relResult: (String, Double)) {
  override def toString = {
    s"""
      |$fact
      |  Predictor: $predictor
      |  Arg1:      $arg1Result
      |  Arg2:      $arg2Result
      |  Rel:       $relResult
    """.stripMargin
  }
}
