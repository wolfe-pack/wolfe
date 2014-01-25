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


  // MLN Starts here
  case class Person(name: Symbol)

  implicit val allPersons = Set(Person('Anna), Person('Bob), Person('Carlos))

  case class Data(smokes: Pred[Person], cancer: Pred[Person], friend: Pred[(Person, Person)])

  implicit val allData = all(Data)

  def mln = linearModel {
    data: Data => {
      import data._
      val f1 = 'smokingIsBad := {p: Person => smokes(p) --> cancer(p)}
      val f2 = 'peerPressure := {
        persons: (Person, Person) =>
          friend(persons._1, persons._2) --> (smokes(persons._1) <-> smokes(persons._2))
      }
      List(f1, f2)
    }
  }

  val weights = Vector('smokingIsBad -> 2.0, 'peerPressure -> 0.01)

  def observation(d: Data) = {
    import d._
    val anna = Person('Anna)
    val bob = Person('Bob)
    val carlos = Person('Carlos)

    smokes(anna) && smokes(bob) &&
      cancer(carlos) && !cancer(bob) &&
      friend(anna, bob) && friend(bob, anna) &&
      friend(anna, carlos) && friend(carlos, anna)
  }

  //  val best = argmax((d: Data) => d.smokes(Person('Anna)))((d: Data) => mln(d, weights))
  //  val best2 = argmax[Data](d => d.smokes(Person('Anna)))(d => mln(d, weights))
  //  val best3 = argmax[Data](_.smokes(Person('Anna)))(mln(_, weights))

  val best = argmax(observation)(mln(_, weights))

  println("Smokes: " + allPersons.filter {best.smokes(_)}.map(_.name))
  println("Cancer: " + allPersons.filter {best.cancer(_)}.map(_.name))
  println("Friends: " + c(allPersons, allPersons).filter {p => best.friend(p._1, p._2)}.map(p => (p._1.name, p._2.name)))

}
