package ml.wolfe.util

import scala.collection.mutable

/**
 * Created by Ingolf Becker on 05/11/2014.
 */
class NelderMeadSimplex extends HyperParameterOptimisationAlgorithm {
  type Param = Map[String, Double]

  override var bestScore     : Double                             = _
  override var iterates      : Seq[(Double, Map[String, Double])] = _
  override var bestParameters: Map[String, Double]                = _
  /**
   * Optimise the problem, starting at the given starting points
   * @param problem The optimisation problem
   * @param startingPoint The starting point of the algorithm
   * @return The set of best parameters
   */
  override def optimise(problem: OptimisationProblem, startingPoint: Param): Param = {
    var dimensions = problem.parametersToOptimize.length

    // this is an implicit ordering, implementing an ordering for the type (Double, Param)
    implicit object MyOrdering extends Ordering[(Double,Map[String,Double])] {
       def compare(x: (Double, Param), y: (Double, Param)): Int = {
         (x._1 - y._1).toInt
       }
    }

    // PriorityQueue searches the current scope for a potential ordering for the type, and will find the implicit ordering above
    // Should have worst point as head, best point as last
    var points: mutable.PriorityQueue[(Double, Param)] = new mutable.PriorityQueue[(Double, Param)]()

    // startingPoints have been modified to form a convex simplex
    val startingPoints = for (key <- startingPoint.keys) yield (startingPoint + (key -> (startingPoint(key) + 1 )))

    // evaluate the starting points
    val evaluatedStartingPoints = for (x <- startingPoints) yield (problem.evaluate(x), x)

    points += ((problem.evaluate(startingPoint), startingPoint))

    points ++= evaluatedStartingPoints
    assert(points.head._1 > points.last._1,"The ordering is wrong")
    def accumulate(acc: Param, element: (Double, Param)): Param = (
      element._2.keys.map(x=> (x, acc(x) + element._2(x))).toMap
    )

    var error = 1.0
    while (error > 1e-6) {
      var centroid = points.foldLeft(startingPoint.keys.map(x=>(x,0.0)).toMap) (accumulate)
      var worstElement = points.dequeue()


      def pointAlongVertex(centroid: Param, worstPoint: Param, scale: Double): Param = (
        worstPoint.keys.map(x => (x, centroid(x) + scale * worstPoint(x))).toMap
      )

      var reflectionPoint = pointAlongVertex(centroid, worstElement._2, -1.0)
      var valueAtReflectionPoint = problem.evaluate(reflectionPoint)

      if ( valueAtReflectionPoint < points.head._1 && points.last._1 <= valueAtReflectionPoint ) {
        // reflected point is neither best or worst in the new simplex
        points += ((valueAtReflectionPoint, reflectionPoint))
      } else if (valueAtReflectionPoint < points.last._1) {
        // relected point is better than the current best, try to go further along that direction
        var secondReflectionPoint = pointAlongVertex(centroid, points.head._2,-2.0)
        var valueAtSecondReflectionPoint = problem.evaluate(secondReflectionPoint)
        if (valueAtSecondReflectionPoint < valueAtReflectionPoint) {
          points += ((valueAtSecondReflectionPoint, secondReflectionPoint))
        } else {
          points += ((valueAtReflectionPoint, reflectionPoint))
        }

      }  else if ( valueAtReflectionPoint > points.head._1) {
        // reflected point is worse than current worst, contract
        var found: Boolean = false
        if (valueAtReflectionPoint < worstElement._1) {
          // try to perform "outside" contraction
          var halfReflectionPoint = pointAlongVertex(centroid, worstElement._2,-0.5)
          var valueAtHalfReflection = problem.evaluate(halfReflectionPoint)
          if (valueAtHalfReflection < valueAtReflectionPoint) {
            points += ((valueAtHalfReflection, halfReflectionPoint))
            found = true
          }
        } else if (!found) {
          // Try to perform "inside" contraction
          var halfReflectionPoint = pointAlongVertex(centroid, worstElement._2, 0.5)
          var valueAtHalfReflection = problem.evaluate(halfReflectionPoint)
          if (valueAtHalfReflection < worstElement._1) {
            points += ((valueAtHalfReflection,halfReflectionPoint))
            found = true
          }
        } else if (!found) {
          // Neither outside nor inside contraction was acceptable, shrink simplex toward best solution

          def averageBetweenTwo(best: Param, other: Param) : Param = (
            other.keys.map(x=> (x, 0.5 * (best(x) + other(x)))).toMap
          )
          points = points.map(x => (problem.evaluate(averageBetweenTwo(points.last._2,x._2)), averageBetweenTwo(points.last._2,x._2)))


        }
      }
      error = math.abs(points.head._1 - points.last._1)

    }
    this.bestParameters = points.last._2
    this.bestScore = points.last._1

    points.last._2

  }

}
