package ml.wolfe.term

import ml.wolfe.WolfeSpec

/**
 * @author riedel
 */
class CaseClassDomSpecs extends WolfeSpec {

  "a case class dom generator" should {
    "generate a case class domain object" in {

      case class Token(word:Int, index:Int)

      implicit val f = 1
      //object Token extends Domain[Token]

    }
  }

}
