package ml.wolfe.term

import ml.wolfe.WolfeSpec

/**
 * @author riedel
 */
class MaxProductSpecs extends WolfeSpec {

  import ml.wolfe.term.TermImplicits._

  "A MaxProduct algorithm" should {
    "optimize a linear chain objective" in {
      val n = 5
      val vars = Range(0, n) map (i => Bools.variable("y" + i))
      val length = Ints(0 until n).Var

      def local(b: BoolTerm) = I(b)
      def pair(b: (BoolTerm, BoolTerm)) = I(b._1 <-> b._2)

      val pairs = vars.dropRight(1) zip vars.drop(1)

      val obj = sum(vars.map(local), length) + sum(pairs.map(pair), length - 1)

      println((obj | length << 5).eval2(true,true,false,true,true))
      val mpParams = MaxProductParameters(10)

      val observation = Settings.fromSeq(Seq(Setting.disc(5)))
      val argmaxer = obj.argmaxBy(ArgmaxerImpl.maxProduct(mpParams)).argmaxerImpl(vars)(observation,null)

      argmaxer.argmax()(Execution(0))

    }
  }


}
