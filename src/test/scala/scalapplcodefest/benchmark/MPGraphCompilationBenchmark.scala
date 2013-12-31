package scalapplcodefest.benchmark

import scalapplcodefest.{TermConverter, MPGraphCompiler, Index}
import scalapplcodefest.term.{Variable, State}
import scalapplcodefest.TermDSL._

class MPGraphCompilationBenchmark extends SimpleBenchmark {

  register(measurement(1000, 50)("name" -> "compiler", "unit" -> "ms") {
    val model = Fixture.linearChain(10, 5)
    val time = BenchmarkTools.time(MPGraphCompiler.compile(model.sig, TermConverter.pushDownConditions(model.body)))
    time / 1000.0
  })

}

object Fixture {
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





