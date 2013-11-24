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
  def feats(): SparseVector

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
    val groupedLambdas = TermConverter.groupLambdasDeep(distConds)
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

  def exhaustiveArgmax(term:Term[Double]) = {
    State.allStates(term.variables.toList).view.maxBy(term.eval(_).right.get)
  }

  def maxProduct(maxIterations: Int)(model: LinearModel)(weights: DenseVector)(instance: State): Inference = {
    import TermImplicits._

    val conditioned = model | instance
    val unrolled = unrollModel(conditioned)

    val aligned = MessagePassingGraphBuilder.build(unrolled, model.weights)

    val result = new Inference {
      private var dirtyState = true
      private var dirtyObjAndFeats = true
      private var _state:State = null
      private var _obj:Double = Double.NegativeInfinity
      private var _feats:SparseVector = null
      private def inferState() {
        if (dirtyState) {
          MaxProduct.run(aligned.graph, maxIterations)
          dirtyState = false
        }
      }
      private def inferObjAndFeatures() {
        if (dirtyObjAndFeats) {
          inferState()
          _feats = new SparseVector(10)
          _obj = MaxProduct.featureExpectationsAndObjective(aligned.graph,_feats)
          dirtyObjAndFeats = false
        }
      }

      def state() = {inferState(); aligned.argmaxState()}
      def obj() = {inferObjAndFeatures(); _obj}
      def feats() = {inferObjAndFeatures(); _feats }
      def updateResult(newWeights: DenseVector) = {
        aligned.graph.weights = weights
        //        println(aligned.graph.toVerboseString(new aligned.FGPrinter(ChunkingExample.key)))
        dirtyState = true
        dirtyObjAndFeats = true
      }
    }
    result.updateResult(weights)
    result
  }
}