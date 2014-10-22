package ml.wolfe.examples

import ml.wolfe.BeliefPropagation
import ml.wolfe.util.Timer

/**
 * @author Sebastian Riedel
 */
object GridExample {

  import ml.wolfe.Wolfe._
  import ml.wolfe.macros.OptimizedOperators._

  type Image = Map[(Int, Int), Boolean]

  def main(args: Array[String]) {
    val n = 10
    val l_w = 1.0
    val g_w = 0.1
    def pixels = (0 until n) x (0 until n)
    def images = maps(pixels, bools)
    def local(x: Image)(y: Image) = sum(pixels) { p => l_w * I(x(p) == y(p)) }
    def vertical(y: Image) = sum(0 until n - 1) { i => sum(0 until n) { j => g_w * I(y(i, j) == y(i + 1, j)) } }
    def horizontal(y: Image) = sum(0 until n) { i => sum(0 until n - 1) { j => g_w * I(y(i, j) == y(i, j + 1)) } }

    @LogZByInference(BeliefPropagation.sumProduct(10))
    def grid(x: Image)(y: Image) = local(x)(y) + vertical(y) + horizontal(y)
    def stats(y: Image) = sum(pixels) { p => oneHot(p, I(y(p))) }

    val x = (pixels map { p => p -> random.nextBoolean() }).toMap
    println("MPE")
    val mpe = Timer.time("mpe") { argmax(images) { grid(x) } }
    println(mpe)
    println("Expect")
    val mu = Timer.time("expect")  {expect(images) { grid(x) } { stats } }
    println(mu)
    println(Timer)


  }

}
