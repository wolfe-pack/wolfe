package scalapplcodefest

import scalapplcodefest.legacy.term._
import scalapplcodefest.util.{Evaluation, EvaluationSummary}

/**
 * State-based generic evaluators.
 * @author Sebastian Riedel
 */
object Evaluator {

  /**
   * Compares the assignment of a variable in one state to the assignment in another.
   * @param gold the gold state (target).
   * @param guess the guess state (prediction).
   * @param v the variable to evaluate
   * @return an evaluation object representing the comparison between to states with respect to the variable.
   */
  def evaluateVariable(gold: State, guess: State, v: Variable[Any]): Evaluation = {
    val t = gold(v)
    val p = guess(v)
    val default = v.default
    val tp = if (t != default && t == p) 1 else 0
    val tn = if (t == default && t == p) 1 else 0
    val fp = if (p != default && t != p) 1 else 0
    val fn = if (t != default && t != p) 1 else 0
    Evaluation(tp, tn, fp, fn)
  }

  def evaluate(instance: State, predictor: State => State,
               variableFilter:Variable[Any] => Boolean,
               keyFunction: Variable[Any] => Any): Map[Any, Evaluation] = {
    val gold = instance.target
    val guess = predictor(instance)
    val variables = guess.domain
    val evals = for (v <- variables.view; if variableFilter(v)) yield keyFunction(v) -> evaluateVariable(gold, guess, v)
    val grouped = evals.groupBy(_._1)
    val summarized = grouped.map({case (key, evalSeq) => key -> evalSeq.map(_._2).reduce(_ + _)})
    summarized
  }

  def defaultKey(variable:Variable[Any]) = variable match {
    case GroundAtom(pred,_) => pred
    case _ => variable
  }

  def defaultFilter(variable:Variable[Any]) = variable match {
    case Target(_) => false
    case _ => true
  }

  def evaluate(instances: Seq[State],
               predictor: State => State,
               variableFilter: Variable[Any] => Boolean = defaultFilter,
               keyFunction: Variable[Any] => Any = defaultKey): EvaluationSummary = {
    val evals = instances.view.map(instance => evaluate(instance, predictor, variableFilter, keyFunction))
    def summarize(evals1: Map[Any, Evaluation], evals2: Map[Any, Evaluation]) = {
      val keys = evals1.keySet ++ evals2.keySet
      val newMap = keys.map(key => key -> (evals1.getOrElse(key, Evaluation()) + evals2.getOrElse(key, Evaluation())))
      newMap.toMap
    }
    val summarized = evals.reduce(summarize)
    EvaluationSummary(summarized)
  }


}



