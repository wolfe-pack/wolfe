package ml.wolfe.term

import ml.wolfe.WolfeSpec

/**
 * @author riedel
 */
class TermMonadSpecs extends WolfeSpec {

  import TermImplicits._

  "The term monad" should {
    "support map operations" in {
      val x = Bools.Variable("x")
      val t = x.map(!_)
      t.eval(x := true) should be(false)
    }
    "support flatMap operations" in {
      val x = Bools.Variable("x")
      val y = Bools.Variable("y")
      val t = for (xv <- x; yv <- y) yield !xv || yv
      for (xv <- Bools; yv <- Bools) {
        t.eval(x := xv, y := yv) should be(!xv || yv)
      }
    }
    "support flatMap operations with repeated terms" in {
      val x = Bools.Variable("x")
      val t = for (xv1 <- x; xv2 <- x) yield !xv1 || xv2
      for (xv <- Bools) {
        t.eval(x := xv) should be(!xv || xv)
      }
    }

  }


}
