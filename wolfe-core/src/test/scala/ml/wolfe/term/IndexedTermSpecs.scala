package ml.wolfe.term

import ml.wolfe.{SimpleIndex, WolfeSpec}

/**
 * @author riedel
 */
class IndexedTermSpecs extends WolfeSpec {

  import ml.wolfe.term.TermImplicits._

  "An indexed term" should {
    "index primitve values" in {

      implicit val indexer = new DefaultIndexer()

      val x = Bools.Var
      val i = indexed(x)

      i.eval(x << true) should be (0)
      i.eval(x << false) should be (1)
      i.eval(x << true) should be (0)


    }
    "index case class values" in {
      @domain case class Data(x:Int,y:Boolean)
      implicit val index = new DefaultIndexer()
      implicit val Datas = Data.Values(Ints,Bools)
      val x = Datas.Var
      val i = indexed(x)

      i.eval(x << Data(1,false)) should be (0)
      i.eval(x << Data(10,false)) should be (1)
      i.eval(x << Data(10,true)) should be (2)
      i.eval(x << Data(10,false)) should be (1)

    }



  }


}
