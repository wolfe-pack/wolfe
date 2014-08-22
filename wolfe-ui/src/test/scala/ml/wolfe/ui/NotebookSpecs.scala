package ml.wolfe.ui

import ml.wolfe.WolfeSpec

/**
 * @author Sebastian Riedel
 */
class NotebookSpecs extends WolfeSpec {

  "A code block source generator" should {
    "return the source string corresponding to the block" in {
      "Test"
      object Block extends CodeBlock {
        val i      = 5
        val j      = 2
        val result = i + j + 2
      }
      val actual = Notebook.block(Block)
      println(actual)
    }
  }

}
