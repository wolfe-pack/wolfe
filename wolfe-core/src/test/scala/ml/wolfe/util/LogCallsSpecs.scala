package ml.wolfe.util

import ml.wolfe.WolfeSpec

/**
 * @author Sebastian Riedel
 */
class LogCallsSpecs extends WolfeSpec {

  "A LogCalls annotation" should {
    "call the function passed into the annotation whenever the annotated function is entered " in {
      var buffer:String = ""
      def log(msg:String) = {
        buffer = msg
        info("log: " + msg)
      }

      def raw(i:Int) = i * i

      @LogCalls(log)
      def myTestMethod(i:Int) = raw(i)

      myTestMethod(4) should be (raw(4))
      buffer should be ("myTestMethod")

      myTestMethod(5)
    }
  }
}

object LogTest extends App {
  def pre(msg:String) = println("entering: " + msg)
  def post(msg:String) = println("leaving: " + msg)

  @LogCalls(pre, post)
  def testMethod1() = {
    //heavy computing...
    Thread.sleep(1000)
    println(3)

    //val x = 100
    println(10)
  }

  @LogCalls(pre)
  def testMethod2() = {
    //nothing to do
  }


  testMethod1()
  testMethod2()
}
