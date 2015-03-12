package ml.wolfe.term

import ml.wolfe.WolfeSpec

/**
 * @author riedel
 */
class MaxProductSpecs extends WolfeSpec {

  import ml.wolfe.term.TermImplicits._

  import Argmaxer._

  "A MaxProduct algorithm" should {
    "optimize a linear chain objective" ignore {
      val n = 5
      val vars = Range(0, n) map (i => Bools.variable("y" + i))
      val length = Ints(0 until n).Var

      def local(b: BoolTerm) = I(b)
      def pair(b: (BoolTerm, BoolTerm)) = I(b._1 <-> b._2)

      val pairs = vars.dropRight(1) zip vars.drop(1)

      val obj = sum(vars.map(local), length) + sum(pairs.map(pair), length - 1)

      println((obj | length << 5).eval2(true, true, false, true, true))
      val mpParams = MaxProductParameters(10)

      val observation = Settings.fromSeq(Seq(Setting.disc(5)))
      val argmaxer = obj.argmaxBy(Argmaxer.maxProduct(mpParams)).argmaxerImpl(vars)(observation, null)

      argmaxer.argmax()(Execution(0))
    }

    "optimize a linear chain objective in high level code" ignore {
      val n = 5
      val Ys = Seqs(Bools, 0, n)
      def model(length: IntTerm)(y: Ys.Term) = {
        sum(0 until length) { i => I(y(i))} +
          sum(0 until length - 1) { i => I(y(i) <-> y(i + 1))}
      }
      val mpParams = MaxProductParameters(10)
      val yStar = argmax2(Ys)(y => model(4)(y) argmaxBy maxProduct(mpParams))
    }

    "optimize a linear chain in a perceptron loss" ignore {
      val mpParams = MaxProductParameters(10)
      val adaParams = AdaGradParameters(100, 0.1)

      val n = 5
      val Xs = Seqs(Ints(0 until 3), 0, n)
      val Ys = Seqs(Bools, 0, n)
      val Weights = Vectors(1000)

      @domain case class Instance(x: Xs.Value, y: Ys.Value)

      implicit val Instances = Instance.Values(Xs, Ys)

      val train = Seq(Instance(IndexedSeq(0, 1, 2), IndexedSeq(true, false, true))).toConst

      def model(x: Xs.Term, w: Weights.Term)(y: Ys.Term) = {
        sum(0 until x.length) { i => w dot oneHot(x(i), I(y(i)))} argmaxBy maxProduct(mpParams)
      }

      def instanceLL(x: Xs.Term, yGold: Ys.Term)(w: Weights.Term) = {
        max(Ys)(y => model(x, w)(y)) - model(x, w)(yGold)
      }

      def ll(w: Weights.Term) = sum(train) { i => instanceLL(i.x, i.y)(w)} argmaxBy adaGrad2(adaParams)

      val wStar = argmax2(Weights)(ll)


    }

  }


}
