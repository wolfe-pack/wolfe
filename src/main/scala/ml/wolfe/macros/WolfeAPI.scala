package ml.wolfe.macros

import com.typesafe.scalalogging.slf4j.Logging

/**
 * This trait defines all operators that Wolfe is optimizing, together
 * with default implementations.
 * @author Sebastian Riedel
 */
trait WolfeAPI extends Logging {

  def argmax[T](data: Iterable[T])(where: T => Boolean)(obj: T => Double): T = {
    data.filter(where).maxBy(obj)
  }

  def argmin[T](data: Iterable[T])(where: T => Boolean)(obj: T => Double): T = {
    data.filter(where).minBy(obj)
  }

  def max[T](data: Iterable[T])(where: T => Boolean)(obj: T => Double): Double = {
    obj(argmax(data)(where)(obj))
  }

  //
  def sum[T, N](data: Iterable[T])(obj: T => N)(implicit num: Numeric[N]): N = {
    data.map(obj).sum(num)
  }

}

object ExperimentalAPI {

  case class Argmax[T](data: Iterable[T], where: T => Boolean = (_: T) => true) {
    def in[A: Iterable] = Argmax[A](implicitly[Iterable[A]])
    def where(pred: T => Boolean) = copy(where = pred)
    def apply(obj: T => Double) = data.filter(where).maxBy(obj)
    def wrt(obj: T => Double) = data.filter(where).maxBy(obj)
    def max(obj: T => Double) = data.filter(where).maxBy(obj)
  }

  def argmax2 = Argmax(Nil)

  def argmax[T: Iterable]: Argmax[T] = Argmax(implicitly[Iterable[T]])

  def arg[T: Iterable]: Argmax[T] = Argmax(implicitly[Iterable[T]])

  def arg2 = Argmax(Nil)


  def argmax2[T](data: Iterable[T])(obj: T => Double, where: T => Boolean = (_: T) => true): T = {
    data.filter(where).maxBy(obj)
  }
  //  def argmax[T](data: Iterable[T])(obj: T => Double): T = {
  //    data.maxBy(obj)
  //  }

  //  def argmax[T: Iterable](where: T => Boolean, obj: T => Double): T = {
  //    implicitly[Iterable[T]].filter(where).maxBy(obj)
  //  }
  //  def argmax[T: Iterable](obj: T => Double): T = {
  //    implicitly[Iterable[T]].maxBy(obj)
  //  }

  def main(args: Array[String]) {
    import ml.wolfe.Wolfe

    implicit val bools = Seq(true, false)
    def obj(b: Boolean) = Wolfe.I(b)
    val t1 = argmax2(bools)(Wolfe.I(_))
    val t2 = (argmax(bools) where (x => x)) {Wolfe.I(_)}
    val t3 = argmax(bools) {x => -Wolfe.I(x)}
    val t4 = argmax[Boolean] max (x => 1.0)
    val t5 = arg(bools) where (x => true) max (x => 1.0)
    val t6 = arg2 in bools where (x => true) max obj
    println(t2)
    println(t3)
    println(t4)
    //    val t3 = argmax2 over bools where (x => x) blah (obj)
    //    val t2 = argmax(bools, (x:Boolean) => x)(Wolfe.I(_))

  }

}


object BruteForceWolfe extends WolfeAPI
