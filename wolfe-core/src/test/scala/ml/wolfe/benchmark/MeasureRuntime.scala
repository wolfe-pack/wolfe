package ml.wolfe.benchmark

import java.util.concurrent.TimeUnit

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


