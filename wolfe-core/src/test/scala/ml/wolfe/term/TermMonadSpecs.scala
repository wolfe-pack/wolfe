package ml.wolfe.term

import ml.wolfe.WolfeSpec

/**
 * @author riedel
 */
class TermMonadSpecs extends WolfeSpec {

  import TermImplicits._

  "The term monad" should {
    "support map operations" in {
      val x = bools.variable("x")
      val t = x.map(!_)
      t.eval(true) should be (false)
    }
    "support flatMap operations" in {
      val x = bools.variable("x")
      val y = bools.variable("y")
      val t = for (xv <- x; yv <- y) yield !xv || yv
      for (xv <- bools; yv <- bools) {
        t.eval(xv,yv) should be (!xv || yv)
      }
    }
    "support flatMap operations with repeated terms" in {
      val x = bools.variable("x")
      val t = for (xv1 <- x; xv2 <- x) yield !xv1 || xv2
      for (xv <- bools) {
        t.eval(xv) should be (!xv || xv)
      }
    }

  }


}
