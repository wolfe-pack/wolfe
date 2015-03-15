package ml.wolfe.term

import ml.wolfe.WolfeSpec

import scala.util.Random

/**
 * @author riedel
 */
class FoldLeftSpecs extends WolfeSpec {

  import ml.wolfe.term.TermImplicits._

  implicit val random = new Random(0)

  "A fold left term" should {
    "do a fold left" ignore {
      @domain case class Result(value: Int)
      implicit val Results = Result.Values(Ints)
      val s = Seq(1, 2, 3).toConst
      //val f = s.foldLeft(Result(0).toConst) { (r, i) => Results.Term(r.value + i )}
      //todo: + i doesn't compile because we require argument to Term be of type Results.value.Term

    }

  }

}
