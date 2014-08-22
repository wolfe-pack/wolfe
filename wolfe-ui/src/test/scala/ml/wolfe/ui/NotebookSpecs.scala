package ml.wolfe.ui

import ml.wolfe.WolfeSpec

/**
 * @author Sebastian Riedel
 */
class NotebookSpecs extends WolfeSpec {

  "A code block source generator" should {
    "return the source string corresponding to the block" in {
      object Block extends CodeBlock {
        val i = 5
        val j = 2
        val result = i + j + 2
      }
      val actual = Notebook.block(Block)
      actual should be (
        """|val i = 5
           |val j = 2
           |i + j + 2""".stripMargin)
    }
  }

}
