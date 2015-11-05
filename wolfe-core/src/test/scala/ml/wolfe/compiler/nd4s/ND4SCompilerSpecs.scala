package ml.wolfe.compiler.nd4s

import ml.wolfe.compiler._
import ml.wolfe.term.WolfeSpec

/**
 * @author riedel
 */
class ND4SCompilerSpecs extends WolfeSpec with CompilerBehaviors {

  "An ND4SCompiler" should {
    supportForwardActivation(ND4SCompiler)
    supportForwardActivationWithComposedValues(ND4SCompiler)
    supportBackwardPass(ND4SCompiler)
  }

}


