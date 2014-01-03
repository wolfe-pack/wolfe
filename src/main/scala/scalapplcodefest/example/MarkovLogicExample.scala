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

    val smokes = data.smokes
    val cancer = data.cancer
    val friends = data.friends

    def f1 = sum(persons) {p => ft('smokingIsBad, smokes(p) -> cancer(p))}

    def f2 = sum(c(persons, persons)) {
      p =>
        ft('peerPressure, friends(p) -> (smokes(p._1) <-> smokes(p._2)))
    }

    (f1 + f2) dot weights
  }

  val weights = Vector('smokingIsBad -> 2.0, 'peerPressure -> 0.0)

  def hidden = for (smokes <- maps(persons, bools);
                    cancer <- maps(persons, bools);
                    friends <- maps(c(persons, persons), bools))
  yield Data(smokes, cancer, friends)

  val prediction2 = argmax(hidden filter {h => h.smokes('Anna) && h.cancer('Anna) && h.friends == map(c(persons,persons), false, ('Anna, 'Bob) -> true)})(y => mln(y, weights))

  println(prediction2)

}
