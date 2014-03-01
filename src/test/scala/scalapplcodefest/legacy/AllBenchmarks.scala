package scalapplcodefest.legacy

import scalapplcodefest.benchmark.BenchmarkSuite

/**
 * Suite of all benchmarks
 * @author Sebastian Riedel
 */
object AllBenchmarks extends BenchmarkSuite {
  add(classOf[MPGraphCompilationBenchmark])
}
