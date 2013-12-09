package scalapplcodefest

import scalapplcodefest.term._

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

case class EvaluationSummary(evaluations:Map[Any,Evaluation]) {
  override def toString = {
    val keys = evaluations.keys.toSeq.sortBy(_.toString)
    val lines = for (key <- keys.view) yield
      f"""
        |------------
        |Key:         $key
        |${evaluations(key)}
      """.stripMargin
    lines.mkString("\n")

  }
}

case class Evaluation(tp: Int = 0, tn: Int = 0, fp: Int = 0, fn: Int = 0) {
  def totalGold = tp + fn
  def totalGuess = tp + fp
  def precision = tp.toDouble / totalGuess
  def recall = tp.toDouble / totalGold
  def f1 = 2.0 * precision * recall / (precision + recall)
  def +(that: Evaluation) = Evaluation(tp + that.tp, tn + that.tn, fp + that.fp, fn + that.fn)
  override def toString =
    f"""Total Gold:  $totalGold
      |Total Guess: $totalGuess
      |Precision:   $precision%f
      |Recall:      $recall%f
      |F1:          $f1%f
    """.stripMargin
}


