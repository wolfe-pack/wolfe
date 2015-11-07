package ml.wolfe.compiler.torch

import ml.wolfe.compiler._
import ml.wolfe.term.WolfeSpec

/**
 * @author riedel
 */
class TorchCompilerSpecs extends WolfeSpec with CompilerBehaviors {

  "A Torch Compiler" should {
    if (TorchZeroMQClient.serverOnline()) {
      supportForwardActivation(TorchCompiler)
      supportForwardActivationWithComposedValues(TorchCompiler)
      supportBackwardPass(TorchCompiler)
      supportForwardPassInMatrixFactorization(TorchCompiler)
    }

  }

}


