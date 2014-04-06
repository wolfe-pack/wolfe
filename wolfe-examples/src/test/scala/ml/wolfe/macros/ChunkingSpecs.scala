package ml.wolfe.macros

import ml.wolfe.{MaxProduct, Wolfe, WolfeSpec}
import cc.factorie.optimize.{Perceptron, OnlineTrainer}
import ml.wolfe.util.{NLP, Evaluator}
import cc.factorie.la.SparseTensor1

/**
 * @author Sebastian Riedel
 */
class ChunkingSpecs extends WolfeSpec {

  "A Chunking Model" should {
    "give reasonable performance on the CoNLL dataset " in {


      import Wolfe._
      import OptimizedOperators._
      import NLP._

      def toToken(conll: Array[String]) = Token(conll(0), Tag(conll(1)), Chunk(conll(2)))

      val train = loadCoNLL("ml/wolfe/datasets/conll2000/train.txt")(toToken).map(Sentence).take(100)

      def Tokens = Wolfe.all(Token)
      def Sentences = Wolfe.all(Sentence)(seqs(Tokens))

      def observed(s: Sentence) = s.copy(tokens = s.tokens.map(_.copy(chunk = hidden)))
      def evidence(s1: Sentence)(s2: Sentence) = observed(s1) == observed(s2)

      def features(s: Sentence) = {
        import s._
        val obs = sum { over(0 until tokens.size) of (i => oneHot('o -> tokens(i).word -> tokens(i).chunk)) }
        val pairs = sum { over(0 until tokens.size - 1) of (i => oneHot('p -> tokens(i).chunk -> tokens(i + 1).chunk)) }
        obs + pairs
      }

      @OptimizeByInference(MaxProduct(_, 1))
      def model(w: Vector)(s: Sentence) = w dot features(s)

      def perceptronLoss(w: Vector)(i: Sentence): Double = max { over(Sentences) of model(w) st evidence(i) } - model(w)(i)

      @OptimizeByLearning(new OnlineTrainer(_, new Perceptron, 4))
      def loss(w: Vector) = sum { over(train) of perceptronLoss(w) }

      val w = argmin { over[Vector] of loss }

      //the predictor given some observed instance
      def predict(s: Sentence) = argmax { over(Sentences) of model(w) st evidence(s) }

      val predictedTrain = map { over(train) using predict }
      val evalTrain = Evaluator.evaluate(train.flatMap(_.tokens), predictedTrain.flatMap(_.tokens))(_.chunk)
      evalTrain.f1 should be(0.85 +- 0.01)

      //      val evalTest = Evaluator.evaluate(test, predictedTest)(_.irisClass)
      //
      //      evalTrain.f1 should be(0.93 +- 0.01)
      //      evalTest.f1 should be(0.98 +- 0.01)


    }


  }

}

object SubtractTest {

  def argument1() = {
    val indices0 = Seq(3841,3196,3855,3866,2422,3877,509,255,3888,509,3897,14,2300,544,332,3723,3074,363,401,434,434,431,401,432,409,401,432,410,410,409,401,431,401,436,451,0,0,0,0,0,0,0)
    val values0 = Seq(1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0)
    val arg0 = new SparseTensor1(1000)
    arg0.ensureCapacity(42)
    for (i <- indices0.indices) arg0 += (indices0(i),values0(i))
    arg0
  }
  def argument2() = {
    val indices0 = Seq(14,255,332,363,389,396,397,399,401,409,420,422,431,432,509,544,2300,2422,3069,3192,3723,3841,3854,3865,3877,3888,3896,0)
    val values0 = Seq(1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,4.0,3.0,1.0,1.0,2.0,2.0,2.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,0.0)
    val arg0 = new SparseTensor1(27)
    arg0.ensureCapacity(28)
    for (i <- indices0.indices) arg0 += (indices0(i),values0(i))
    arg0
  }

  def main(args: Array[String]) {
    val arg1 = argument1()
    val arg2 = argument2()
    println(arg1)
    println(arg2)
    val result1 = arg1 - arg2
    val result2 = new SparseTensor1(arg1.size + arg2.size)
    result2 += arg1
    result2 -= arg2
    val result3 = arg1 - arg2

    println("----")

    println(result1)
    println(result2)
    println(result3)

    println("----")

    result1.activeDomainSize
    result2.activeDomainSize
    result3.activeDomainSize

    println(result1)
    println(result2)
    println(result3)

    println("----")
    println(arg1)
    println(arg2)

    println(arg1.dot(arg2))


  }
}

/*

-----
SparseIndexedTensor npos=25 sorted=25 ind=14,255,332,363,401,409,410,431,432,434,436,451,509,544,2300,2422,3074,3196,3723,3841,3855,3866,3877,3888,3897,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0 val=1.0,1.0,1.0,1.0,5.0,2.0,2.0,2.0,2.0,2.0,1.0,1.0,2.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0

SparseIndexedTensor npos=27 sorted=27 ind=14,255,332,363,389,396,397,399,401,409,420,422,431,432,509,544,2300,2422,3069,3192,3723,3841,3854,3865,3877,3888,3896,0 val=1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,4.0,3.0,1.0,1.0,2.0,2.0,2.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,0.0

SparseIndexedTensor npos=52 sorted=0 ind=14,255,332,363,401,409,410,431,432,434,436,451,509,544,2300,2422,3074,3196,3723,3841,3855,3866,3877,3888,3897,14,255,332,363,389,396,397,399,401,409,420,422,431,432,509,544,2300,2422,3069,3192,3723,3841,3854,3865,3877,3888,3896,0,0,0,0,0,0,0,0,0,0,0 val=1.0,1.0,1.0,1.0,5.0,2.0,2.0,2.0,2.0,2.0,1.0,1.0,2.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,-1.0,-1.0,-1.0,-1.0,-1.0,-1.0,-1.0,-1.0,-4.0,-3.0,-1.0,-1.0,-2.0,-2.0,-2.0,-1.0,-1.0,-1.0,-1.0,-1.0,-1.0,-1.0,-1.0,-1.0,-1.0,-1.0,-1.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0

SparseIndexedTensor npos=62 sorted=0 ind=3841,3196,3855,3866,2422,3877,509,255,3888,509,3897,14,2300,544,332,3723,3074,363,401,434,434,431,401,432,409,401,432,410,410,409,401,431,401,436,451,14,255,332,363,389,396,397,399,401,409,420,422,431,432,509,544,2300,2422,3069,3192,3723,3841,3854,3865,3877,3888,3896,0 val=1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,-1.0,-1.0,-1.0,-1.0,-1.0,-1.0,-1.0,-1.0,-4.0,-3.0,-1.0,-1.0,-2.0,-2.0,-2.0,-1.0,-1.0,-1.0,-1.0,-1.0,-1.0,-1.0,-1.0,-1.0,-1.0,-1.0,-1.0,0.0
 */