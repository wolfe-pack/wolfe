package ml.wolfe.benchmark

import java.util.concurrent.TimeUnit
import ml.wolfe.legacy.{TermConverter, MPGraphCompiler}
import ml.wolfe.legacy.TermDSL._
import ml.wolfe.legacy.term.{Variable, State}
import ml.wolfe.Index

/**
 * Created by larysa  30.12.13
 */


object MeasureRuntime {

  def execTimeOf[A](f: => A) = {
    val start = System.nanoTime
    val result = f
    val time: Long = TimeUnit.MILLISECONDS.convert((System.nanoTime - start), TimeUnit.NANOSECONDS)
    println("execution time in milliseconds: " + time)
    result
  }

}

object Fixture1 {
  def linearChain(sentenceLength: Int, labelCount: Int) = {
    val key = new Index
    val n = 'n of ints
    val k = 'k of ints
    val labels = 0 ~~ k
    val tokens = 0 ~~ n
    val label = 'label of tokens |-> labels
    val word = 'word of tokens |-> strings
    val weights = 'weights of vectors
    val bias = vectors.sum(for (i <- tokens) yield unit(key('bias, label(i))))
    val emission = vectors.sum(for (i <- tokens) yield unit(key('emission, label(i), word(i))))
    val trans = vectors.sum(for (i <- 0 ~~ (n - 1)) yield unit(key('trans, label(i), label(i + 1))))
    val model = (bias + emission + trans) dot weights
    val words = Range(0, sentenceLength).map(i => word.atom(i) -> "A word").toMap
    val condition = State(words.asInstanceOf[Map[Variable[Any], Any]]) + state(n -> sentenceLength, k -> labelCount)
    lam(label, model | condition)
  }
}

object RunBench {
  def main(args: Array[String]) {
    def timeMyOperation1 = {
      val model = Fixture1.linearChain(0, 5)
      MPGraphCompiler.compile(model.sig, TermConverter.pushDownConditions(model.body))
    }
    def timeMyOperation2 = {
      val model = Fixture1.linearChain(10, 5)
      MPGraphCompiler.compile(model.sig, TermConverter.pushDownConditions(model.body))
    }

    MeasureRuntime.execTimeOf(timeMyOperation1)
    MeasureRuntime.execTimeOf(timeMyOperation2)
  }
}