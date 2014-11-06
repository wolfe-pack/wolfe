package ml.wolfe.util

import ml.wolfe.WolfeSpec

/**
 * @author Sebastian Riedel
 */
class LogCallsSpecs extends WolfeSpec {

  "A LogCalls annotation" should {
//    "call the function passed into the annotation whenever the annotated function is entered " in {
//      var buffer:String = ""
//      def log(msg:String) = {
//        buffer = msg
//        info("log: " + msg)
//      }
//
//      def raw(i:Int) = i * i
//
//      @LogCalls(log)
//      def myTestMethod(i:Int) = raw(i)
//
//      myTestMethod(4) should be (raw(4))
//      buffer should be ("myTestMethod")
//
//      myTestMethod(5)
//    }
//
//    "call a method when entering and another when exiting" in {
//      var preCounter = 0
//      var postCounter = 0
//
//      def pre(msg: String) = {
//        info(s"entering: $msg")
//        preCounter += 1
//      }
//      def post(msg: String) = {
//        info(s"exiting: $msg")
//        postCounter += 1
//      }
//
//      @LogCalls(pre)
//      def testMethod1() = 3.7
//
//      @LogCalls(pre, post)
//      def testMethod2() = {
//        preCounter -= 1
//        "hi"
//      }
//
//      def checkCounter(pre: Int, post: Int) {
//        preCounter shouldBe pre
//        postCounter shouldBe post
//      }
//
//      checkCounter(0, 0)
//
//      testMethod1() shouldBe 3.7
//      checkCounter(1, 0)
//
//      testMethod2() shouldBe "hi"
//      checkCounter(1, 1)
//
//
//
//      println(testMethod1())
//      println(testMethod2())
//    }
  }
}