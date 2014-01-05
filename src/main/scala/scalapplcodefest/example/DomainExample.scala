package scalapplcodefest.example

import scalapplcodefest.Wolfe


/**
 * @author Sebastian Riedel
 */
object DomainExample extends App {

  import Wolfe._

  def argmax[T](condition:T => Boolean)(obj: T => Double)(implicit domain: Set[T]) = domain.maxBy(obj)
  def sum[T, D](obj: T => D)(implicit domain: Set[T], num: Numeric[D]) = domain.view.map(obj).sum(num)

  case class Person(name: Symbol)
  case class Data(smokes: Pred[Person], cancer: Pred[Person], friend: Pred[(Person, Person)])

  implicit val allPersons = Set(Person('Anna), Person('Bob))
  implicit val allData = all(Data)

  def mln(data: Data, weights: Vector) = {

    import data._
    val f1 = sum {(p: Person) => ft('smokingIsBad, smokes(p) -> cancer(p))}
    val f2 = sum {(p1: Person, p2: Person) => ft('peerPressure, friend(p1, p2) -> (smokes(p1) <-> smokes(p2)))}
    (f1 + f2) dot weights
  }

  val weights = Vector('smokingIsBad -> 2.0, 'peerPressure -> 0.0)

  val best = argmax((d: Data) => d.smokes(Person('Anna))) ((d:Data) => mln(d, weights))
  val best2 = argmax[Data](d=> d.smokes(Person('Anna))) (d => mln(d, weights))
  val best3 = argmax[Data](_.smokes(Person('Anna))) (mln(_, weights))

  //
  //  val best2 = argmax (allData filter (d => d.smokes(Person('Anna)))) (d => 1.0)
  //  val best3 = argmax2 ((d:Data) => 1.0) (allData filter (d => d.smokes(Person('Anna))))


  println(allData.mkString("\n"))


}
