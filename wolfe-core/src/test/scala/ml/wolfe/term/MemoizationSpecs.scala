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
      val x = Ints.Var
      val s = mem(sampleUniform(0 until 10000) + x)
      val t1 = (s | x << 1) === (s | x << 1)
      val t2 = (s | x << 1) === (s | x << 2)
      t1.eval() should be (true)
      t2.eval() should be (false)
    }

  }

}
