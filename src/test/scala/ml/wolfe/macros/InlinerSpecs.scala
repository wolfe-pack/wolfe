package ml.wolfe.macros

import org.scalatest.{Matchers, WordSpec}

/**
 * @author Sebastian Riedel
 */
class InlinerSpecs extends WordSpec with Matchers  {

  "An inliner" should {
    "inline functions in the scope" in {
      def f(x:Int) = x
      val actual = Inliner.inlineMacro(f(5),1)
      actual should be ("Some(5)")
    }
    "inline functions only once" in {
      def f(x:Int) = x
      def g(x:Int) = f(x)
      val actual = Inliner.inlineMacro(g(5),1)
      actual should be ("Some(f(5))")
    }
    "inline functions twice" in {
      def f(x:Int) = x
      def g(x:Int) = f(x)
      val actual = Inliner.inlineMacro(g(5),2)
      actual should be ("Some(5)")
    }

    "inline functions with implicits" in {
      def f(y:Int)(implicit i:Int) = y + i
      implicit val x = 5
      def k(j:Int) = f(j)
      val actual = Inliner.inlineMacro(k(1),1)
      actual should be ("Some(f(1)(x))")
    }
    "return None if no inlining was performed because no symbols to inline were found" in {
      val actual = Inliner.inlineMacro(5,1)
      actual should be ("None")
    }

    "should inline methods without parameters " in {
      def k = 5
      val actual = Inliner.inlineMacro(k,1)
      actual should be ("Some(5)")
    }

    "should inline generic methods" in {
      def f[T](t:T) = t
      def k[T](t:T) = f(t)
      val actual1 = Inliner.inlineMacro(k(5),1)
      val actual2 = Inliner.inlineMacro(k(5),2)
      actual1 should be ("Some(f[T](5))")
      actual2 should be ("Some(5)")
    }

//    "blah" in {
//      import ml.wolfe.Wolfe._
//      case class Data(value:Boolean)
//      def features(data:Data) = oneHot(data.value, 1.0)
//      def model(weights:Vector)(data:Data) = features(data) dot weights
//      def sampleSpace = all2(Data)(bools)
//
//      val actual = Inliner.inlineMacro(sampleSpace,1)
//      println(actual)
//    }

  }

}
