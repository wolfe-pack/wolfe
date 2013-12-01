package scalapplcodefest

/**
 * An inference made by some algorithm. This interfaces supports both marginal inference
 * and MAP inference.
 * @author Sebastian Riedel
 */
trait Inference {
  /**
   * The state corresponding to this inference. For example, this can be the most likely state, or the
   * state maximizing the per-variable marginal probabilities.
   * @return the state corresponding to the inference made.
   */
  def state(): State

  /**
   * An objective associated with the inference. Can be the log-linear score, or a log partition function etc.
   * @return objective associated with inference.
   */
  def obj(): Double

  /**
   * A feature vector associated with the inference. Can be the expectation of the feature function
   * under the model, or the feature vector of the argmax state.
   * @return feature representation associated with the inference.
   */
  def feats(): Vector

}

trait MutableInference extends Inference {
  /**
   * Change the weights and update the inference.
   * @param newWeights new weight vector to use.
   */
  def updateResult(newWeights: DenseVector)
}

object Inference {

  def unrollModel(term:Term[Double]) = {
    val flattenQuantified = TermConverter.flatten(term, Math.VecAdd)
    val distConds = TermConverter.distConds(flattenQuantified)
    val groupedLambdas = TermConverter.groupLambdas(distConds)
    val distDots = TermConverter.distDots(groupedLambdas)
    val unrolled = TermConverter.unrollLambdas(distDots)
    val flatten = TermConverter.flatten(unrolled, Math.DoubleAdd)

    //    println(conditioned)
    //    println(flattenQuantified)
    //    println(distConds)
    //    println(groupedLambdas)
    //    println(distDots)
    //    println(unrolled)
    //    println(flatten)

    flatten
  }

  def exhaustiveArgmax(term:Term[Double]):Inference = {
    val argmaxState = State.allStates(term.variables.toList).view.maxBy(term.eval(_).right.get)
    val featTerm = term match {
      case Linear(feats,_,_) => feats
      case Conditioned(Linear(feats,_,_),_) => feats
      case _ => Constant(new SparseVector(0))
    }
    new Inference {
      def state() = argmaxState
      def obj() = term.eval(argmaxState).right.get
      def feats() = featTerm.eval(argmaxState).right.get
    }
  }
  
  def maxProductArgmax(maxIterations:Int)(term:Term[Double]):Inference = {
    val (weightVar,inner, weights) = term match {
      case Conditioned(withInstance@Conditioned(LinearModel(f,w,b),_),s) => (w,withInstance,s(w))
      case _ => (null,term,null)
    }
    val unrolled = unrollModel(inner)
    val aligned = MessagePassingGraphBuilder.build(unrolled, weightVar)
    aligned.graph.weights = weights.asInstanceOf[DenseVector]
    println(aligned.graph.toVerboseString(new aligned.FGPrinter(ChunkingExample.key)))
    MaxProduct.run(aligned.graph, maxIterations)
    val argmaxState = aligned.argmaxState()
    val argmaxFeats = new SparseVector(10)
    val argmaxScore = MaxProduct.featureExpectationsAndObjective(aligned.graph,argmaxFeats)

    new Inference {
      def state() = argmaxState
      def obj() = argmaxScore
      def feats() = argmaxFeats
    }

  }

}