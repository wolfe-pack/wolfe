package ml.wolfe.term

import ml.wolfe.WolfeSpec
import ml.wolfe.util.Math._

/**
 * @author rockt
 */
class DomSpec extends WolfeSpec {
  import ml.wolfe.term.TermImplicits._

  "A domain" should {
    "initialize vectors randomly" in {
      val dom = Vectors(2)
      def nextRandom = random.nextGaussian() * 0.1
      val init = dom.createRandomSetting(nextRandom)
      val v = init.vect(0)
      v(0) shouldNot be(v(1))
    }
  }
}
