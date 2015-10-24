package ml.wolfe.term

import ml.wolfe.{Tensor, Language}
import ml.wolfe.compiler.ND4SCompiler
import org.scalautils.Good

/**
 * @author riedel
 */
class ND4SCompilerSpecs extends WolfeSpec {

  import Language._

  "An ND4SCompiler" should {
    "support forward evaluation of matrix vector multiplication" ignore {
      val W = Var[Tensor]
      val x = Var[Tensor]
      val term = sigmoid(W * x)

      val module = ND4SCompiler.compile(term)
      module.init(W := ???)
      module.forward(x := ???)
      module.output() should be (null)
    }

    "support backward evaluation of matrix vector multiplication" ignore {
      val W = Var[Tensor]
      val x = Var[Tensor]
      val term = sigmoid(W * x)

      val module = ND4SCompiler.compile(term)
      module.init(W := ???)
      module.forward(x := ???)
      module.backward(???)
      module.gradient(W) should be (null)
      //module.gradient(x) should be ??? if x is cont.
    }

  }
}
