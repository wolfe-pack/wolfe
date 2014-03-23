package ml.wolfe.macros

/**
 * @author Sebastian Riedel
 */
object ExperimentalAPI {

  case class Argmax[T](data: Iterable[T], where: T => Boolean = (_: T) => true) {
    def over[A: Iterable] = Argmax[A](implicitly[Iterable[A]])
    def where(pred: T => Boolean) = copy(where = pred)
    def apply(obj: T => Double) = data.filter(where).maxBy(obj)
    def wrt(obj: T => Double) = data.filter(where).maxBy(obj)
    def of(obj: T => Double) = data.filter(where).maxBy(obj)
  }

  def argmax2 = Argmax(Nil)

  def argmax[T: Iterable]: Argmax[T] = Argmax(implicitly[Iterable[T]])



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
    val t4 = argmax[Boolean] of (x => 1.0)
    val t6 = argmax2 over bools where (x => true) of obj
    println(t2)
    println(t3)
    println(t4)
    //    val t3 = argmax2 over bools where (x => x) blah (obj)
    //    val t2 = argmax(bools, (x:Boolean) => x)(Wolfe.I(_))

  }

}

