package ml.wolfe.term

import ml.wolfe.WolfeSpec

/**
 * @author riedel
 */
class NameProviderSpecs extends WolfeSpec {

  object Implicits extends NameProviderImplicits

  object Name {
    def name(implicit provider:NameProvider) = provider.newName()
  }


  "A name provider" should {
    "should find the name of the enclosing variable owner" in {
      import Implicits._
      import Name._
      val n = name
      n should be ("n")
    }
  }

  "A domain" should {
    "use the implicit name provider for create variable names" in {
      import TermImplicits._
      val x = bools.Var
      x.name should be ("x")
    }
  }

}
