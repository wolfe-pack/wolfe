package ml.wolfe.fg20

import cc.factorie.la.DenseTensor1
import ml.wolfe.WolfeSpec

/**
 * @author riedel
 */
class NeuralSpecs extends WolfeSpec {

  "A sigmoid unit" should {
    "return score 0 if provided outputs match calculated output" in {
      val in = new VectVar(2, "out")
      val out = new VectVar(2, "in")
      val sig = new SigmoidUnit(in, out)

      val setting = sig.createPartialSetting(State(Map(
        in -> new DenseTensor1(Array(1.0, 2.0)),
        out -> new DenseTensor1(Array(SigmoidUnit.sigmoid(1.0), SigmoidUnit.sigmoid(2.0))))))

      val actual = sig.scorer().score(setting)
      actual should be (0.0)
    }
  }

}
