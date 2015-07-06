package ml.wolfe.term

import ml.wolfe.WolfeSpec

import scala.util.Random

/**
 * @author riedel
 */
class TermMonadSpecs extends WolfeSpec {

  import TermImplicits._

  "The term monad" should {
    "support map operations" in {
      val x = Bools.Var
      val t = x convertValue (!_)
      t.eval(x := true) should be(false)
    }

    "return the same mapped value on the same execution" in {
      val random = new Random(0)
      val y = Ints.Var
      val m = y convertValue (_ + random.nextInt())
      val t = m === m
      t.eval(y := 0) should be (true)
    }

    "support flatMap operations" in {
      val x = Bools.Variable("x")
      val y = Bools.Variable("y")
      val t = x convertValues(xv => y convertValue (yv => !xv || yv))
      for (xv <- Bools; yv <- Bools) {
        t.eval(x := xv, y := yv) should be(!xv || yv)
      }
    }

    "support flatMap operations with repeated terms" in {
      val x = Bools.Variable("x")
      val t = x convertValues(xv1 => x convertValue (xv2 => !xv1 || xv2))
      for (xv <- Bools) {
        t.eval(x := xv) should be(!xv || xv)
      }
    }

    "behave within an argmax" in {
      val X = Seqs(Seq(1,2).toDom, 5)
      def foo(x:X.Term) = sum(0 until 5) { i =>
        x(i).convertValue{ v => v * 2.0 }
      }
      val bar = argmax(X)(foo) by Argmaxer.bruteForce
      bar.eval().toList should be (List(2, 2, 2, 2, 2))
    }

  }


}
