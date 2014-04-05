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

  def argmax3[T](data: Iterable[T])(where: T => Boolean)(obj: T => Double): T = {
    data.filter(where).maxBy(obj)
  }
  def argmax3[T: Iterable](obj: T => Double): T = {
    implicitly[Iterable[T]].maxBy(obj)
  }

  //  def argmax4[T](data: Iterable[T])(where: T => Boolean)(obj: T => Double): T = {
  //    data.filter(where).maxBy(obj)
  //  }

  def argmax4[T](data: Iterable[T])(obj: T => Double): T = {
    data.maxBy(obj)
  }
  def argmax4[T: Iterable](where: T => Boolean)(obj: T => Double): T = {
    implicitly[Iterable[T]].filter(where).maxBy(obj)
  }

  def argmax5[T](data: Iterable[T])(where: T => Boolean = (_:T) => true, of: T => Double): T = {
    data.filter(where).maxBy(of)
  }
  def argmax5[T: Iterable](where: T => Boolean = (_:T) => true, of: T => Double): T = {
    implicitly[Iterable[T]].filter(where).maxBy(of)
  }

  def argmax6[T](data: Iterable[T])(of: T => Double,where: T => Boolean = (_:T) => true): T = {
    data.filter(where).maxBy(of)
  }
  def argmax6[T: Iterable](of: T => Double,where: T => Boolean = (_:T) => true): T = {
    implicitly[Iterable[T]].filter(where).maxBy(of)
  }

  def argmax7[T](data:Iterable[T], where:T => Boolean = (_:T) => true)(obj:T => Double) = {
    data.filter(where).maxBy(obj)
  }

  def argmax7[T:Iterable](where:T => Boolean = (_:T) => true)(obj:T => Double) = {
    implicitly[Iterable[T]].filter(where).maxBy(obj)
  }
  def argmax7[T:Iterable](obj:T => Double) = {
    implicitly[Iterable[T]].maxBy(obj)
  }

  //  def argmax5[T: Iterable](obj: T => Double): T = {
//    implicitly[Iterable[T]].maxBy(obj)
//  }

  //  def argmax4[T: Iterable](obj: T => Double): T = {
  //    implicitly[Iterable[T]].maxBy(obj)
  //  }

  //  def argmax3[T](data: Iterable[T])(obj: T => Double): T = {
  //    data.maxBy(obj)
  //  }


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
    import scala.language.implicitConversions

    implicit val bools = Seq(true, false)
    implicit val symbols = Seq('A, 'B)
    def obj(b: Boolean) = Wolfe.I(b)
    def pred(b: Boolean) = b
    val t1 = argmax2(bools)(Wolfe.I(_))
    val t2 = (argmax(bools) where (x => x)) {Wolfe.I(_)}
    val t3 = argmax(bools) {x => -Wolfe.I(x)}
    val t4 = argmax[Boolean] of (x => 1.0)
    val t6 = argmax2 over bools where (x => true) of obj
    val t7 = argmax2
    val t8 = argmax3(bools)(pred)(obj)
    val t9 = argmax4(symbols)(x => 1.0)
    val t10 = argmax4((x:Symbol) => true)((x:Symbol) => 1.0)
    val t11 = argmax5(symbols)(x => true, x => 1.0)
    val t12 = argmax5((x:Symbol) => true, (x:Symbol) => 1.0)
    val t13 = argmax5(symbols)(of = x => 1.0)
    val t14 = argmax5(of = (x:Symbol) => 1.0)
    val t15 = argmax6(symbols)(x => 1.0)
    val t16 = argmax6(symbols)(x => 1.0,x => true)
    val t17 = argmax6((x:Symbol) => 1.0, (x:Symbol) => true)
    val t18 = argmax6((x:Symbol) => 1.0, (x:Symbol) => true)
    val t19 = argmax7(symbols, (x:Symbol) => true)(x => 1.0)
    val t20 = argmax7((x:Symbol) => 1.0)
    val t21 = argmax7((x:Symbol) => true)((x:Symbol) => 1.0)

    implicit def convert1[A](a:A):Set[A] = Set(a)

    println(t2)
    println(t3)
    println(t4)
    //    val t3 = argmax2 over bools where (x => x) blah (obj)
    //    val t2 = argmax(bools, (x:Boolean) => x)(Wolfe.I(_))

  }

}

