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

    "should inline methods without parameters " in {
      def k = 5
      val actual = CodeRepository.inlineMacro(k,1)
      actual should be ("Some(5)")
    }

    "should inline generic methods" in {
      def f[T](t:T) = t
      def k[T](t:T) = f(t)
      val actual1 = CodeRepository.inlineMacro(k(5),1)
      val actual2 = CodeRepository.inlineMacro(k(5),2)
      actual1 should be ("Some(f[T](5))")
      actual2 should be ("Some(5)")
    }

    "should work with a toolbox" in {
      val repository = CodeRepository.fromToolbox(GlobalToolbox.toolBox)
      repository.addCodeString("def k(x:Int) = x")
      repository.addCodeString("def f(x:Int) = k(x)")
      val tree = repository.addCodeString("f(5)")
      val actual = repository.inlineOnce(tree)
      val expected = repository.addCodeString("k(5)")
      expected.equalsStructure(actual.get) should be (true)
      println(tree)
      println(actual)
      println(expected)
      println(expected.equalsStructure(actual.get))
      println(expected.equalsStructure(actual.get))

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
