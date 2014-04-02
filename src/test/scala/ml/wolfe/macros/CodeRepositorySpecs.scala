package ml.wolfe.macros

import org.scalatest.Matchers
import ml.wolfe.WolfeSpec

/**
 * @author Sebastian Riedel
 */
class CodeRepositorySpecs extends WolfeSpec {

  "A code repository" should {
    "inline functions in the scope" in {
      def f(x:Int) = x
      val actual = CodeRepository.inlineMacro(f(5),1)
      actual should be ("Some(5)")
    }
    "inline functions only once" in {
      def f(x:Int) = x
      def g(x:Int) = f(x)
      val actual = CodeRepository.inlineMacro(g(5),1)
      actual should be ("Some(f(5))")
    }
    "inline functions twice" in {
      def f(x:Int) = x
      def g(x:Int) = f(x)
      val actual = CodeRepository.inlineMacro(g(5),2)
      actual should be ("Some(5)")
    }

    "inline functions with implicits" in {
      def f(y:Int)(implicit i:Int) = y + i
      implicit val x = 5
      def k(j:Int) = f(j)
      val actual = CodeRepository.inlineMacro(k(1),1)
      actual should be ("Some(f(1)(x))")
    }
    "return None if no inlining was performed because no symbols to inline were found" in {
      val actual = CodeRepository.inlineMacro(5,1)
      actual should be ("None")
    }

    "inline methods without parameters " in {
      def k = 5
      val actual = CodeRepository.inlineMacro(k,1)
      actual should be ("Some(5)")
    }

    "inline generic methods" in {
      def f[T](t:T) = t
      def k[T](t:T) = f(t)
      val actual1 = CodeRepository.inlineMacro(k(5),1)
      val actual2 = CodeRepository.inlineMacro(k(5),2)
      actual1 should be ("Some(f[T](5))")
      actual2 should be ("Some(5)")
    }

    "inline functions defined in objects " in {
      object Functions {
        def f(i:Int) = i
      }
      val actual = CodeRepository.inlineMacro(Functions.f(1),1)
      actual should be ("Some(1)")
    }


  }

}
