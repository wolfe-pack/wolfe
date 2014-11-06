package ml.wolfe.util

import scala.collection.mutable
import cc.factorie.app.nlp.coref.CorefFeatures.True

/**
 * Implementation of Powell's method for derivative free optimisation
 * The main idea is the algorithm 9.3 from Numerical Optimization by Wright and Nocedal.
 * Following as closely as possible the scipy version:
 * https://github.com/scipy/scipy/blob/maintenance/0.14.x/scipy/optimize/optimize.py
 * @author Andreas Vlachos
 */
class Powell extends HyperParameterOptimisationAlgorithm {
  type Param = Map[String, Double]

  override var bestScore     : Double                             = Double.PositiveInfinity
  override var iterates      : mutable.Buffer[(Double, Param)] = mutable.Buffer()
  override var bestParameters: Param                = Map()

  /**
   * Return the linear interpolation between the starting point and the direction
   * that minimizes the function value
   * @param problem: the function we are trying to optimize
   * @param point: the starting point
   * @param direction: the direction (a vector with one 1)
   * @return the point that minimizes the function along the line defined by the point and the direction
   */
  def linesearch(problem: OptimisationProblem, point:Param, direction:Param) : Param = ???

  /**
   * Optimise the problem, starting at the given starting points
   * @param problem The optimisation problem
   * @param startingPoint The starting point of the algorithm
   * @return The set of best parameters
   */
  override def optimise(problem: OptimisationProblem, startingPoint: Param): Param = {
    val dimensions = problem.parametersToOptimize.length

    println(f"Starting optimisation with $dimensions%d dimensions.")
    var points: mutable.ArrayBuffer[(Double, Param)] = new mutable.ArrayBuffer[(Double, Param)]()
    // these are the directions to be tried
    // essentially, an one-hot per dimension
    // essentially it is something that is easy to add

    def genNewDirection(keyToChange:String) : Param = {
      (for (key <- startingPoint.keys) yield (key, if (key == keyToChange) 1.0 else 0.0)).toMap
    }

    // define the conjugate directions
    var directions: mutable.ArrayBuffer[Param] = new mutable.ArrayBuffer[Param]()
    // populate them initially
    for (key <- startingPoint.keys){
      directions.append(genNewDirection(key))
    }

    val startingPointEval:Double = problem.evaluate(startingPoint)

    points += ((startingPointEval, startingPoint))
    // just initialize the error
    var diff = 1.0
    // convergence criterion
    while (diff > 1e-14) {
      // initialize the point vector to be used in this iteration
      // we do not need the values
      var pointsInIteration :  mutable.ArrayBuffer[Param] = new mutable.ArrayBuffer[Param]()
      // get the most recent iterate
      pointsInIteration += points.last._2
      // for each direction
      for (direction <- directions){
        // for each direction find the step away from the current iterate that minimizes the function value
        val tempPoint = linesearch(problem, pointsInIteration.last, direction)
        pointsInIteration += (tempPoint)
      }
      // remove the first one
      directions.remove(0)
      // add the new one, z_(n+1) - z_1
      directions.append((for (key <- startingPoint.keys) yield (key, pointsInIteration.last(key) - pointsInIteration(0)(key) )).toMap)

      val newPoint = linesearch(problem, pointsInIteration.last, directions.last)
      val newPointEval:Double = problem.evaluate(newPoint)
      points += ((newPointEval, newPoint))

      println(f"new iterate: ${newPoint}%s score ${newPointEval}%.8f")
      // update the diff
      diff  = math.abs(newPointEval - points.last._1)
    }

    this.bestParameters = points.minBy(_._1)._2
    this.bestScore = points.minBy(_._1)._1
    this.iterates += points.minBy(_._1)
    points.minBy(_._1)._2

  }

}
