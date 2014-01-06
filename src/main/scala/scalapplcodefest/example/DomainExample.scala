package scalapplcodefest.example

import scalapplcodefest.Wolfe


/**
 * @author Sebastian Riedel
 */
object DomainExample extends App {

  import Wolfe._

  def argmax[T](condition: T => Boolean)(obj: T => Double)(implicit domain: Set[T]) = domain.filter(condition).maxBy(obj)
  def sum[T, D](obj: T => D)(implicit domain: Set[T], num: Numeric[D]) = domain.view.map(obj).sum(num)

  def formula[T, D](obj: T => D)(implicit domain: Set[T], num: Numeric[D]) = sum(obj)(domain, num)

  implicit class RichAny(s: Any) {
    def :=[T](formula: T => Boolean)(implicit domain: Set[T]): Vector = {
      sum {p: T => ft(s, formula(p))}
    }
  }

  def score(features: List[Vector], weights: Vector) = weights dot features.sum

  // MLN Starts here
  case class Person(name: Symbol)

  case class Data(smokes: Pred[Person], cancer: Pred[Person], friend: Pred[(Person, Person)])

  implicit val allPersons = Set(Person('Anna), Person('Bob))
  implicit val allData = all(Data)

  def mln(data: Data, weights: Vector) = {

    import data._
    val f1 = 'smokingIsBad := {p: Person => smokes(p) -> cancer(p)}
    val f2 = 'peerPressure := {
      persons: (Person, Person) =>
        friend(persons._1, persons._2) -> (smokes(persons._1) <-> smokes(persons._2))
    }

    score(List(f1, f2), weights)
  }

  val weights = Vector('smokingIsBad -> 2.0, 'peerPressure -> 0.0)

  val best = argmax((d: Data) => d.smokes(Person('Anna)))((d: Data) => mln(d, weights))
  val best2 = argmax[Data](d => d.smokes(Person('Anna)))(d => mln(d, weights))
  val best3 = argmax[Data](_.smokes(Person('Anna)))(mln(_, weights))

  //
  //  val best2 = argmax (allData filter (d => d.smokes(Person('Anna)))) (d => 1.0)
  //  val best3 = argmax2 ((d:Data) => 1.0) (allData filter (d => d.smokes(Person('Anna))))

  //  println(allData.mkString("\n"))

  println(best.smokes)
  println(best.cancer)
  println(best.friend)

}
