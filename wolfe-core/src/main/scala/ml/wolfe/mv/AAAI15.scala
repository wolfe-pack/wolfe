package ml.wolfe.mv

/**
 * @author Sebastian Riedel
 */
object AAAI15 {

  sealed trait MyList[+T]
  case class Cons[T](h:T,t:MyList[T]) extends MyList[T]
  case object MyNil extends MyList[Nothing]
  type MyMap[A,B] = MyList[(A,B)]
  case class World(smokes:MyMap[String,Boolean], cancer:MyMap[String,Boolean])

  val persons = List("Bob","Anna")

  def listGet[T](index:Int, list:MyList[T]):T = list match {
    case MyNil => sys.error("Index out of bounds")
    case Cons(h,t) => if (index == 0) h else listGet(index - 1, t)
  }

  def mapGet[A,B](key:A, map:MyMap[A,B]):B = map match {
    case MyNil => sys.error("Key not in map")
    case Cons((k,v),t) => if (key == k) v else mapGet(key,t)
  }

  //todo: do a tail recursive version and make sure it works
  def allMaps[A,B](dom:List[A],range:List[B]):List[MyMap[A,B]] = dom match {
    case Nil => List(MyNil)
    case h :: t => for (m <- allMaps(t,range); v <- range) yield Cons((h,v),m)
  }

  def sum[T](dom:List[T])(obj:T => Double):Double = dom match {
    case Nil => 0.0
    case h :: t => obj(h) + sum(t)(obj)
  }

  def dot[K](keys: List[K])(arg1: MyMap[K,Double], arg2: MyMap[K,Double]) = {
    sum(keys) { k => mapGet(k,arg1) * mapGet(k,arg2) }
  }

  val keys = List("w")

  def I(pred:Boolean) = if (pred) 1.0 else 0.0

  def model(y:World) =
    sum(persons) { p => I(!mapGet(p,y.smokes) || mapGet(p,y.cancer))}

  def feat(y:World) = Cons("w" -> 1.0, MyNil)

  def linear(w:MyMap[String,Double])(y:World) =
    dot(keys)(w,feat(y))


}
