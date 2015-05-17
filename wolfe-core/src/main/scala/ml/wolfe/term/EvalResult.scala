package ml.wolfe.term

class EvalResult[T](val value:T, val evaluator:Evaluator) {
  override def toString = value.toString

  def factorGraphs = {
    evaluator.tree.collect {
      case e: Term[_]#ArgmaxEvaluator => e.maxer match {
        case m:MaxProductBP => m.fg
      }
    }
  }

}