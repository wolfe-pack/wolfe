package ml.wolfe.term

import ml.wolfe.WolfeSpec

import scala.util.Random

/**
 * @author riedel
 */
class AllSettingsIterableSpecs extends WolfeSpec {

  import ml.wolfe.term.TermImplicits._

  implicit val random = new Random(0)

  "An all settings iterable" should {
    "should iterate over all settings" in {
      val d1 = Bools
      val d2 = Seqs(Discretes("Yo"), 2)
      val d3 = Ints(1 until 3)
      val iterable = AllSettings(IndexedSeq(d1, d2, d3)) {
        s => (d1.toValue(s(0)), d2.toValue(s(1)), d3.toValue(s(2)))
      }
      val expected = Set(
        (false, Vector("Yo","Yo"),1),
        (false, Vector("Yo","Yo"),2),
        (true, Vector("Yo","Yo"),1),
        (true, Vector("Yo","Yo"),2)
      )
      iterable.toSet should be (expected)
    }
  }
}
