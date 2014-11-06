package ml.wolfe.util

import org.scalatest.{Matchers, WordSpec}
import scala.math._

/**
 * @author Andreas Vlachos
 */
class NelderMeadSpecs extends WordSpec with Matchers {

  class ToyProblem extends OptimisationProblem {
    override val parametersToOptimize: Seq[HyperParameter] = Seq(new HyperParameter("A"), new HyperParameter("B"), new HyperParameter("C"))
    /**
     * Evaluate the optimisation problem given the set of hyper parameters.
     * @param hyperparameters The map of hyper parameters
     * @return The score of this evaluation, higher is better
     */
    override def evaluate(hyperparameters: Map[String, Double]): Double = {
      /*
    Python code from scipy
        self.F = array([[1,1,1],[1,1,0],[1,0,1],[1,0,0],[1,0,0]])
        self.K = array([1., 0.3, 0.5])
        log_pdot = dot(self.F, x)
        logZ = log(sum(exp(log_pdot)))
        f = logZ - dot(self.K, x)
        return f
 */
      //println(hyperparameters)

      val Fs: Seq[Map[String, Double]] = Seq(
        Map("A" -> 1.0, "B" -> 1.0, "C" -> 1.0),
        Map("A" -> 1.0, "B" -> 1.0, "C" -> 0.0),
        Map("A" -> 1.0, "B" -> 0.0, "C" -> 1.0),
        Map("A" -> 1.0, "B" -> 0.0, "C" -> 0.0),
        Map("A" -> 1.0, "B" -> 0.0, "C" -> 0.0))

      val Ks: Map[String, Double] = Map("A" -> 1.0, "B" -> 0.3, "C" -> 0.5)

      // F*hyperparam
      //println("values")
      //println(for (f <- Fs) yield
      //  (for (key <- hyperparameters.keys.toSeq) yield hyperparameters(key)*f(key)))

      val log_pdot = (for (f <- Fs) yield
        for (key <- hyperparameters.keys.toSeq) yield hyperparameters(key)*f(key)).sum
      // log(sum(exp(log_pdot)))
      //println(log_pdot)
      val logZ = log((for (value <- log_pdot) yield exp(value)).sum)
      // K*x
      val Kx = for (key <- hyperparameters.keys.toSeq) yield hyperparameters(key) * Ks(key)
      //println(logZ)
      //println(Kx)
      //println(Kx.sum)
      //println(logZ - Kx.sum)
      logZ - Kx.sum
    }
  }
  class RosenbrockProblem(hyperparams:Map[String, Double]) extends OptimisationProblem {
    val parametersToOptimize =
      (for (paramName <- hyperparams.keys) yield new HyperParameter(paramName)).toSeq

    /**
     * Evaluate the Rosenbrock function:
     * return sum(100.0*(x[1:]-x[:-1]**2.0)**2.0 + (1-x[:-1])**2.0)
     * @param hyperparameters The map of hyper parameters
     * @return The score of this evaluation, higher is better
     */
    override def evaluate(hyperparameters: Map[String, Double]): Double = {

      val values = hyperparameters.values.toArray
      var result = 0.0
      // iterate over all 1..N-1
      for (index <- 0 until values.size -1){
        //println(index)
        result += pow(1- values(index), 2.0)
        result += 100*pow(values(index+1) - pow(values(index), 2.0), 2.0)

      }
      result
    }
  }


  // This implements the unit test of SciPy
  "NelderMeadSimplex" should {
    "find the correct solution for the toy Berger et al. (1996) problem" in {
      val toyProblem: OptimisationProblem = new ToyProblem()
      val myOptimizer: HyperParameterOptimisationAlgorithm = new NelderMeadSimplex()

      val correctSolution = toyProblem.evaluate(Map("A" -> 0.0, "B" -> -0.524869316, "C" -> 0.487525860))
      // Set the starting point to 0
      val zeros: Map[String, Double] = Map("A" -> 0, "B" -> 0, "C" -> 0)
      myOptimizer.optimise(toyProblem, zeros)
      println(f"Error is ${correctSolution - myOptimizer.bestScore}%g")
      myOptimizer.bestScore should be(correctSolution +- 1e-6)

    }

    "find the correct solution for the Rosenbrock function" in {

      val startingPoint: Map[String, Double] = Map("A" -> 1.3, "B" -> 0.7, "C" -> 0.8, "D" -> 1.9, "E" -> 1.2)
      val solutionPoint: Map[String, Double] = startingPoint.keys.map((_,1.0)).toMap
      val toyProblem: OptimisationProblem = new RosenbrockProblem(startingPoint)
      val correctSolution = toyProblem.evaluate(solutionPoint)

      val myOptimizer: HyperParameterOptimisationAlgorithm = new NelderMeadSimplex()

      myOptimizer.optimise(toyProblem, startingPoint)
      myOptimizer.bestScore should be(correctSolution +- 1e-6)
    }
  }
}

/*
  "A simple index should" should (

  )*/

  /*
    "A simple index" should {
      behave like anIndex(new SimpleIndex)
    }

    "A hierarchical index" should {
      behave like anIndex(new HierarchicalIndex)
    }
  */




