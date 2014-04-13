package ml.wolfe


/**
 * @author Sebastian Riedel
 */
class IndexSpecs extends WolfeSpec {

  def anIndex(newIndex: => Index) {
    "assign fresh indices to fresh keys, starting at index 0 and in incremental fashion" in {
      val index = newIndex
      val i1 = index.index("A")
      val i2 = index.index("B")
      i1 should be (0)
      i2 should be (1)
      val inverse = index.inverse()
      inverse(0) should be ("A")
      inverse(1) should be ("B")
    }
  }

  "A simple index" should {
    behave like anIndex(new SimpleIndex)
  }

  "A hierarchical index" should {
    behave like anIndex(new HierarchicalIndex)
  }



}
