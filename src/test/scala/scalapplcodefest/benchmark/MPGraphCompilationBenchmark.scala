package scalapplcodefest.benchmark

import org.scalameter.api._
import scalapplcodefest.{TermConverter, MPGraphCompiler, Index}
import scalapplcodefest.term.{Variable, State}


object MPGraphCompilationBenchmark extends PerformanceTest.Regression {

  import scalapplcodefest.TermDSL._

  def persistor = new SerializationPersistor("target")


  override def reporter = Reporter.Composite(
    new RegressionReporter(
      RegressionReporter.Tester.OverlapIntervals(),
      RegressionReporter.Historian.ExponentialBackoff()),
    HtmlReporter(true)
  )
  val lengths: Gen[Int] = Gen.range("length")(5, 10, 5)

  val models = for (l <- lengths) yield linearChain(l, 5)

  performance of "MPGraph compilation" in {
    measure method "compile" in {
      using(models) config(
        exec.maxWarmupRuns -> 10,
        exec.benchRuns -> 10,
        exec.independentSamples -> 10
        ) in {
        model =>
          MPGraphCompiler.compile(model.sig, TermConverter.pushDownConditions(model.body))
      }
    }

  }

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

