package ml.wolfe.term

import ml.wolfe.WolfeSpec

import scala.util.Random

/**
 * @author riedel
 */
class MemoizationSpecs extends WolfeSpec {

  import ml.wolfe.term.TermImplicits._

  implicit val random = new Random(0)

  "A memoization term" should {
    "return different value if input is different" in {
      def obj(i: IntTerm) = {
        mem(i)
      }
      val t = sum(Seq(1,2,1).toConst) { i => obj(i).toDouble}
      t.eval2() should be (4)
    }

  }

}
