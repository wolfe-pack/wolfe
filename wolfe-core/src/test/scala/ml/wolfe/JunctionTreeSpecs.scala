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

    "Return the same argmax as normal BP on a tree" in {
      def sqDiff(x:Int, y:Int) = (x-y) * (x-y)

      @OptimizeByInference(BeliefPropagation(_, 1))
      def modelBP(s:Seq[Int]) =  sum(0 until s.length-1) { i:Int => sqDiff(s(i), s(i+1)) }

      @OptimizeByInference(BeliefPropagation.onJunctionTree(_))
      def modelJT(s:Seq[Int]) =  sum(0 until s.length-1) { i:Int => sqDiff(s(i), s(i+1)) }
      //todo: Sebastian, why doesn' this work? ArrayOps throws a NullPointerException.
      val t = BruteForceOperators.argmax(seqs(1 to 3, 5)) {modelBP}
      val u = argmax(seqs(1 to 3, 5)) {modelJT}

      modelBP(t) shouldEqual modelJT(u)
    }

    "Return the same argmax as brute force on a loopy graph" in {
      def sqDiff(x:Int, y:Int) = (x-y) * (x-y)
      @OptimizeByInference(BeliefPropagation.onJunctionTree(_))
      def model(s:Seq[Int]) =  sum(0 until s.length-1) { i:Int => sqDiff(s(i), s(i+1)) } +
                           2 * sum(0 until s.length-2) { i:Int => sqDiff(s(i), s(i+2)) }

      val t = BruteForceOperators.argmax(seqs(1 to 3, 5)) {model}
      val u = argmax(seqs(1 to 3, 5)) {model}

      model(t) shouldEqual model(u)
    }

  }

}
