package ml.wolfe.util

import scala.collection.mutable

/**
 * Implementation of the Nelder-Mead Algorithm for derivative free optimisation
 * @author Ingolf Becker
 */
class NelderMeadSimplex extends HyperParameterOptimisationAlgorithm {
  type Param = Map[String, Double]

  override var bestScore     : Double                             = throw new InstantiationException("Optimise first!")
  override var iterates      : mutable.Buffer[(Double, Param)] = throw new InstantiationException("Optimise first!")
  override var bestParameters: Param                = throw new InstantiationException("Optimise first!")
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

    // startingPoints have been modified to form a convex simplex
    def modifyStartingPoint(start : Double): Double =
      if (start == 0.0) 0.00025 else 0.05 + start

    val startingPoints = for (key <- startingPoint.keys) yield startingPoint + (key -> modifyStartingPoint(startingPoint(key)))

    // evaluate the starting points
    val evaluatedStartingPoints = for (x <- startingPoints) yield (problem.evaluate(x), x)

    points += ((problem.evaluate(startingPoint), startingPoint))

    points ++= evaluatedStartingPoints

    def accumulate(acc: Param, element: (Double, Param)): Param =
      element._2.keys.map(x=> (x, acc(x) + element._2(x) / dimensions)).toMap

    var error = 1.0
    while (error > 1e-12) {
      val worstElement = points.maxBy(_._1)
      points -= worstElement

      val centroid = points.foldLeft(startingPoint.keys.map(x => (x, 0.0)).toMap)(accumulate)
      val bestElement = points.minBy(_._1)
      this.iterates += bestElement
      val secondWorstElement = points.maxBy(_._1)

      println(f"bestElement: ${bestElement._1}%.8f at ${bestElement._2("A")}%.8f, ${bestElement._2("B")}%.8f, ${bestElement._2("C")}%.8f")

      def pointAlongVertex(centroid: Param, worstPoint: Param, scale: Double): Param =
        worstPoint.keys.map(x => (x, centroid(x) + scale * (worstPoint(x) - centroid(x)))).toMap

      val reflectionPoint = pointAlongVertex(centroid, worstElement._2, -1.0)
      val valueAtReflectionPoint = problem.evaluate(reflectionPoint)

      if ( valueAtReflectionPoint < secondWorstElement._1 && bestElement._1 <= valueAtReflectionPoint ) {
        // reflected point is neither best or worst in the new simplex
        points += ((valueAtReflectionPoint, reflectionPoint))
      } else if (valueAtReflectionPoint < bestElement._1) {
        // relected point is better than the current best, try to go further along that direction
        val secondReflectionPoint = pointAlongVertex(centroid, worstElement._2, -2.0)
        val valueAtSecondReflectionPoint = problem.evaluate(secondReflectionPoint)
        if (valueAtSecondReflectionPoint < valueAtReflectionPoint) {
          points += ((valueAtSecondReflectionPoint, secondReflectionPoint))
        } else {
          points += ((valueAtReflectionPoint, reflectionPoint))
        }

      }  else {
        // reflected point is worse than current worst, contract
        var found: Boolean = false
        if (valueAtReflectionPoint < worstElement._1) {
          // try to perform "outside" contraction
          val halfReflectionPoint = pointAlongVertex(centroid, worstElement._2, -0.5)
          val valueAtHalfReflection = problem.evaluate(halfReflectionPoint)
          if (valueAtHalfReflection < valueAtReflectionPoint) {
            points += ((valueAtHalfReflection, halfReflectionPoint))
            found = true
          }
        } else {
          // Try to perform "inside" contraction
          val halfReflectionPoint = pointAlongVertex(centroid, worstElement._2, 0.5)
          val valueAtHalfReflection = problem.evaluate(halfReflectionPoint)
          if (valueAtHalfReflection < worstElement._1) {
            points += ((valueAtHalfReflection,halfReflectionPoint))
            found = true
          }
        }
        if (!found) {
          // Neither outside nor inside contraction was acceptable, shrink simplex toward best solution

          def averageBetweenTwo(best: Param, other: (Double,Param)) : (Double, Param) =
            if (best == other._2) {
              other
            } else {
              val newpoint = best.keys.map(x => (x, 0.5 * (best(x) + other._2(x)))).toMap
              (problem.evaluate(newpoint), newpoint)
            }


          points += worstElement
          points = points.map(x => averageBetweenTwo(bestElement._2,x))
        }
      }
      error = math.abs(points.maxBy(_._1)._1 - points.minBy(_._1)._1)

    }
    this.bestParameters = points.minBy(_._1)._2
    this.bestScore = points.minBy(_._1)._1
    this.iterates += points.minBy(_._1)
    points.minBy(_._1)._2

  }

}
