package scalapplcodefest.benchmark

import org.scalameter.api._
import scalapplcodefest.{TermConverter, MPGraphCompiler, Index}
import scalapplcodefest.term.{Variable, State}
import org.scalameter._
import org.scalameter.execution.SeparateJvmsExecutor
import org.scalameter.api.Executor
import org.scalameter.api.Persistor
import org.scalameter.api.Executor
import org.scalameter.Executor
import org.scalameter.Setup
import org.scalameter.api.Gen
import org.scalameter.api.PerformanceTest
import org.scalameter.api.Reporter
import java.io.File
import java.security.AccessController
import sun.security.action.GetPropertyAction


object MPGraphCompilationBenchmark extends PerformanceTest.Regression {

  import scalapplcodefest.TermDSL._

  def persistor = new SerializationPersistor("target")
  //  def persistor = new DummyPersistor


  override def reporter = Reporter.Composite(
    new RegressionReporter(
      RegressionReporter.Tester.OverlapIntervals(),
      RegressionReporter.Historian.ExponentialBackoff()),
    HtmlReporter(true)
  )

  override def executor: Executor =
    new MySeperateJVMExecutor(SeparateJvmsExecutor(warmer, aggregator, measurer))


  val lengths: Gen[Int] = Gen.range("length")(5, 10, 5)

  val models = for (l <- lengths) yield linearChain(l, 5)

  performance of "MPGraph compilation" in {
    measure method "compile" in {
      using(models) config(
        exec.maxWarmupRuns -> 2,
        exec.benchRuns -> 2,
        exec.independentSamples -> 2
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

class DummyPersistor extends Persistor {
  def load(context: Context) = {
    println(System.getProperty("java.io.tmpdir"))
    println("Loading...")
    println(context.curve)
    println(context.scope)
    History(Seq.empty)
  }
  def save(context: Context, h: History) = {
    println("Saving...")
    println(context.curve)
    println(context.scope)
    println(h.toString())
  }
}

class MySeperateJVMExecutor(executor: SeparateJvmsExecutor) extends Executor {
  def runSetup[T](setup: Setup[T]) = {
    //System.setProperty("java.io.tmpdir", "target/")
    val tmpdir = new File(AccessController.doPrivileged(new GetPropertyAction("java.io.tmpdir")))
    println(System.getProperty("java.io.tmpdir"))
    println(tmpdir.getAbsolutePath)
    val tmp = File.createTempFile("blah", "blub")
    println(tmp.getAbsolutePath)
    executor.runSetup(setup)
  }
}
