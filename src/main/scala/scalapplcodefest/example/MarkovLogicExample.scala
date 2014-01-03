package scalapplcodefest.example

import scalapplcodefest._

/**
 * The smokes & cancer MLN.
 *
 * @author Sebastian Riedel
 */
object MarkovLogicExample extends App {

  import Wolfe._

  type Person = Symbol

  case class Data(smokes: Person => Boolean, cancer: Person => Boolean, friends: Map[(Person, Person), Boolean])

  val persons = Set('Anna, 'Bob)
  val bools = Set(true, false)

  def mln(data: Data, weights: Vector) = {

    import data._

    def f1 = sum(persons) {p => ft('smokingIsBad, smokes(p) -> cancer(p))}

    def f2 = sum(c(persons, persons)) {
      case (p1, p2) => ft('peerPressure, friends(p1, p2) -> (smokes(p1) <-> smokes(p2)))
    }

    (f1 + f2) dot weights
  }

  val weights = Vector('smokingIsBad -> 2.0, 'peerPressure -> 0.0)

  def hidden = for (smokes <- maps(persons, bools);
                    cancer <- maps(persons, bools);
                    friends <- maps(c(persons, persons), bools))
  yield Data(smokes, cancer, friends)

  //h.friends only ("Anna","Bob")
  val prediction2 = argmax(hidden filter {h => h.smokes('Anna) && h.cancer('Anna) && h.friends == map(c(persons, persons), false, ('Anna, 'Bob) -> true)})(y => mln(y, weights))
  val prediction3 = argmax(hidden filter {h => h.smokes('Anna) && h.cancer('Anna) && (h.friends only (('Anna,'Bob)))})(y => mln(y, weights))

  println(prediction2)

}
