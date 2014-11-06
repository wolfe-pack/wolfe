package ml.wolfe.util

import ml.wolfe.WolfeSpec
import scala.math._

/**
 * Created by Ingolf Becker on 05/11/2014.
 */
class NelderMeadSpecs extends WolfeSpec {

  class ToyProblem extends OptimisationProblem{
    override val parametersToOptimize: Seq[HyperParameter] = Seq(new HyperParameter("A"), new HyperParameter("B"),new HyperParameter("C"))
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
      val Fs: Seq[Map[String, Double]] = Seq(Map("A" -> 1.0, "B" -> 1.0, "C" -> 1.0),
      Map("A" -> 1.0, "B" -> 1.0, "C" -> 0.0),
      Map("A" -> 1.0, "B" -> 0.0, "C" -> 1.0),
      Map("A" -> 1.0, "B" -> 0.0, "C" -> 0.0),
      Map("A" -> 1.0, "B" -> 0.0, "C" -> 0.0))

      val Ks: Map[String, Double] = Map("A" -> 1.0, "B" -> 0.3, "C" -> 0.5)

      //var log_pdot : Map[String, Double] = Map("A" -> 0.0, "B" -> 0.0, "C" -> 0.0)

      //for (key <- hyperparameters.keys){
      //  for (f <- Fs) {
      //    log_pdot(key) += hyperparameters(key) * f(key)
      //  }
      //}

      val log_pdot = (for (key <- hyperparameters.keys) yield (key , (for (f <- Fs) yield hyperparameters(key) * f(key)).sum)).toMap


      var Z = 0.0
      var Kx = 0.0

      for (key <- hyperparameters.keys){
        Z += exp(log_pdot(key))
        Kx += Ks(key) * hyperparameters(key)
      }

      log(Z) - Kx

    }
  }

  // This implements the unit test of SciPy
  "NelderMeadSimplex" should {
    "find the correct solution" in {
     val toyProblem : OptimisationProblem = new ToyProblem()
     val myOptimizer : HyperParameterOptimisationAlgorithm = new NelderMeadSimplex()


     // Set the starting point to 0
     val zeros: Map[String, Double] = Map("A" -> 0, "B" -> 0, "C" -> 0)
     myOptimizer.optimise(toyProblem, zeros)
     val solutionFound = myOptimizer.bestParameters
     solutionFound("A") should be(0.0)
     solutionFound("B") should be(-0.524869316)
     solutionFound("C") should be(0.487525860)

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




