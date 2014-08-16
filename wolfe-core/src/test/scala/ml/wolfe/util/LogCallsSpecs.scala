package ml.wolfe.util

import ml.wolfe.WolfeSpec

/**
 * @author Sebastian Riedel
 */
class LogCallsSpecs extends WolfeSpec {

  "A LogCalls annotation" should {
    "call the function passed into the annotation whenever the annotated function is entered " in {
      var buffer:String = ""
      def log(msg:String) {buffer = msg}
      def raw(i:Int) = i * i
      @LogCalls(log)
      def myTestMethod(i:Int) = raw(i)
      myTestMethod(4) should be (raw(4))
      buffer should be ("myTestMethod")
    }
  }

}
