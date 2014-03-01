package ml.wolfe.legacy

import ml.wolfe.benchmark.BenchmarkSuite

/**
 * Suite of all benchmarks
 * @author Sebastian Riedel
 */
object AllBenchmarks extends BenchmarkSuite {
  add(classOf[MPGraphCompilationBenchmark])
}
