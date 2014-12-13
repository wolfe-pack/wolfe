package ml.wolfe.util

import scala.collection.mutable
/**
 * The trait to be implemented of an Optimisation algorithm
 * Created by Ingolf Becker on 05/11/2014.
 */
trait HyperParameterOptimisationAlgorithm {

  /**
   * the best score after optimising
   */
  var bestScore: Double

  /**
   * The best set of parameters found
   */
  var bestParameters: Map[String, Double]

  /**
   * Sequence of intermediate steps, each a tuple of score and parameters at that step
   */
  var iterates: mutable.Buffer[(Double, Map[String, Double])]

  /**
   * Optimise the problem, starting at the given starting points
   * @param problem The optimisation problem
   * @param startingPoint The starting point of the algorithm
   * @return The set of best parameters
   */
  def optimise(problem: OptimisationProblem, startingPoint: Map[String, Double]): Map[String, Double]

  /**
   * To be implemented
   */
  def printConvergenceGraph(): Unit = ???

}

/**
 * The trait an optimisation problem needs to expose in order to have its hyper parameters optimised
 */
trait OptimisationProblem {
  /**
   * The list of parameters to be optimised
   */
  val parametersToOptimize: Seq[HyperParameter]

  /**
   * Evaluate the optimisation problem given the set of hyper parameters.
   * @param hyperparameters The map of hyper parameters
   * @return The score of this evaluation, higher is better
   */
  def evaluate(hyperparameters: Map[String, Double]): Double

}



