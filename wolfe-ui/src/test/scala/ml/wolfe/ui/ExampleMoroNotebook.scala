package ml.wolfe.ui

import java.io.File

/**
 * @author Sebastian Riedel
 */
object ExampleMoroNotebook extends MutableMoroNotebook with App {

  md(
    """
      |## This is a heading
      |
      |This is a paragraph.
    """)

  source(
    """
      |3 + 4
    """)

  section("New Slide")

  md(
    """
      |More text here...
    """)
  val dir = new File("/Users/sriedel/projects/moro-notebooks/test")
  dir.mkdirs()
  saveTo("Example Notebook", new File(dir,"example.json"))

}
