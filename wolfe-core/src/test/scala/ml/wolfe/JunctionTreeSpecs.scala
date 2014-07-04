package ml.wolfe

import macros.OptimizedOperators
/**
 * @author luke
 */
class JunctionTreeSpecs extends WolfeSpec {
  import OptimizedOperators._
  import Wolfe._
  import macros.Library._

  "Junction Tree Belief Propagation" should {
    "Return the same argmax as brute force on a loopy graph" in {

      def sqDiff(x:Int, y:Int) = (x-y) * (x-y)
      @OptimizeByInference(BeliefPropagation.onJunctionTree(_))
      def model(s:Seq[Int]) =  sum(0 until s.length-1) { i:Int => sqDiff(s(i), s(i+1)) } +
                           2 * sum(0 until s.length-2) { i:Int => sqDiff(s(i), s(i+2)) }

      /*def obs(s:Seq[Int]) = s.map{x:Int => 0}
      val s5 = Seq(0,0,0,0,0)*/

      val t = BruteForceOperators.argmax(seqs(1 to 3, 5)) {model}
      val u = argmax(seqs(1 to 3, 5)) {model}

      model(t) shouldEqual model(u)
    }
  }

}
